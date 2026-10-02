rm(list = ls())
graphics.off()

library(readxl);library(rio) #importing and exporting data
library(dplyr);library(tidyr) #for dealing with datas
library(factoextra) #for k-mean clustering
library(rio) #to import and export data
library(gridExtra);library(grid)
library(lubridate) #processing time series data
library(scales)
library(RColorBrewer)
library(ggplot2)

## Load the data
EU_sales_data <- read_xlsx("C:\\Users\\Admin\\Documents\\Data Got Talent\\Final round\\All data in one sheet.xlsx", sheet = "All data") %>% 
  as.data.frame() %>%
  filter(SalesTerritoryGroup == "Europe" & ProductCategoryKey == 1)

EU_sales_data %>% names()
EU_sales_data %>% summary()

## Extract needed data
EU_sales_data <- EU_sales_data %>% dplyr::select(CustomerKey,OrderDate,SalesAmount, Order)

#Deal with the data
EU_sales_data$CustomerKey <- EU_sales_data$CustomerKey %>% as.factor()

EU_sales_data$OrderDate <- EU_sales_data$OrderDate %>% ymd()

## ---Data preparation - FRM model
(analysis_date <- max(EU_sales_data$OrderDate)) #Analysis_date is the final Order Date in the dataset 2020-01-28

EU_FRM_data <- EU_sales_data %>%
  group_by(CustomerKey) %>%
  summarise(
    Length = as.numeric(analysis_date - min(OrderDate)),
    Frequency = sum(Order),
    Recency = as.numeric(analysis_date - max(OrderDate)),
    Monetary = sum(SalesAmount))

#Check for duplicates
EU_FRM_data %>% filter(duplicated(EU_FRM_data$CustomerKey)) #A tibble: 0 x 5 means that the data have 0 row and 5 cols, meaning no duplicated data

#Let's take a glimpse on the FRM data
EU_FRM_data
EU_FRM_data %>% summary()
dim(EU_FRM_data) #The data has 2752 rows representing 2752 customers, and 4 columns)

## ---Preprocessing data for k-kmean analysis
EU_kmean_scaled_data <- EU_FRM_data[,2:5] %>% scale()

#find the best number of cluster
fviz_nbclust(EU_kmean_scaled_data, kmeans, method = "wss") # > k = 6 clusters

#run the analysis
EU_kmean6 <- kmeans(EU_kmean_scaled_data, centers = 6, iter.max = 100, nstart = 100)

fviz_cluster(EU_kmean6, data = EU_kmean_scaled_data)

#appending clustering result to cleaned data
EU_kmean_result_export <- EU_FRM_data %>% 
  as.data.frame() %>%
  dplyr::mutate(Cluster_6 = EU_kmean6$cluster)

#Preparing data for final output
EU_kmean_result <- EU_kmean_result_export %>%
  as.data.frame() %>%
  group_by(Cluster_6) %>% 
  summarise(Count = n(),
            Length = median(Length),
            Frequency = median(Frequency), #The value of Frequency is the MEDIAN value of Frequency for each cluster group  
            Recency = median(Recency), #The value of Recency is the MEDIAN value of Recency for each cluster group  
            Monetary = median(Monetary), #The value of Monetary is the MEDIAN value of Monetary for each cluster group  
            Length.cmt = "0",
            Frequency.cmt = "0",
            Recency.cmt = "0", #Just prepare data for further classification
            Monetary.cmt = "0",
            Final.cmt = "0")

for(i in 1:dim(EU_kmean_result)[1]){
  
  if(EU_kmean_result$Length[i] <= quantile(EU_kmean_result$Length, 1) &
     EU_kmean_result$Length[i] > quantile(EU_kmean_result$Length, 0.666)){
    EU_kmean_result$Length.cmt[i] <- "La" #Length above avg
  } else {
    if(EU_kmean_result$Length[i] <= quantile(EU_kmean_result$Length, 0.666) &
       EU_kmean_result$Length[i] > quantile(EU_kmean_result$Length, 0.2)){
      EU_kmean_result$Length.cmt[i] <- "Lm" #Length middle avg 
    } else {
      EU_kmean_result$Length.cmt[i] <- "Lb" #Length below average
    }}
  
  if(EU_kmean_result$Monetary[i] <= quantile(EU_kmean_result$Monetary, 1) &
     EU_kmean_result$Monetary[i] > quantile(EU_kmean_result$Monetary, 0.666)){
    EU_kmean_result$Monetary.cmt[i] <- "Ma" #Monetary above avg
  } else {
    if(EU_kmean_result$Monetary[i] <= quantile(EU_kmean_result$Monetary, 0.666) &
       EU_kmean_result$Monetary[i] > quantile(EU_kmean_result$Monetary, 0.333)){
      EU_kmean_result$Monetary.cmt[i] <- "Mm" #Monetary middle avg 
    } else {
      EU_kmean_result$Monetary.cmt[i] <- "Mb" #Monetary below average
    }}
  
  if(EU_kmean_result$Recency[i] <= quantile(EU_kmean_result$Recency, 1)&
     EU_kmean_result$Recency[i] > quantile(EU_kmean_result$Recency, 0.666)) {
    EU_kmean_result$Recency.cmt[i] <- "Ra" #Recency above average
  } else {
    if(EU_kmean_result$Recency[i] <= quantile(EU_kmean_result$Recency, 0.666) &
       EU_kmean_result$Recency[i] > quantile(EU_kmean_result$Recency, 0.333)) {
      EU_kmean_result$Recency.cmt[i] <- "Rm" #Recency middle average
    } else {
      EU_kmean_result$Recency.cmt[i] <- "Rb" #Recency below average
    }}
  
  if(EU_kmean_result$Frequency[i] <= quantile(EU_kmean_result$Frequency, 1) &
     EU_kmean_result$Frequency[i] > quantile(EU_kmean_result$Frequency, 0.666)){
    EU_kmean_result$Frequency.cmt[i] <- "Fa" #Frequency above avg
  } else {
    if(EU_kmean_result$Frequency[i] <= quantile(EU_kmean_result$Frequency, 0.666) &
       EU_kmean_result$Frequency[i] > quantile(EU_kmean_result$Frequency, 0.333)){
      EU_kmean_result$Frequency.cmt[i] <- "Fm" #Frequency middle avg  
    } else {
      EU_kmean_result$Frequency.cmt[i] <- "Fb" #Frequency below average
    }}
  
  EU_kmean_result$Final.cmt[i] <- paste(EU_kmean_result$Length.cmt[i],
                                        EU_kmean_result$Frequency.cmt[i],
                                        EU_kmean_result$Recency.cmt[i],
                                        EU_kmean_result$Monetary.cmt[i]) #Summarise the attributes of each cluster group
}

## Visualization
color_scheme <- c('#FFD369','#FFD31D','#FC4F4F',"#95CD41",'#CDB30C','#64CCDA','#185ADB')

cluster_viz <- function(input){
  
  count <- EU_kmean_result %>%
    ggplot(aes(x = as.factor(input),
               y = Count,
               fill = as.factor(input))) +
    stat_summary(fun = mean, #summarizing y
                 geom = "bar")+ #with bars
    theme_bw()+
    labs(x = "",
         y = "# of Customer",
         title = "Number of Customer") +
    geom_label(aes(label = Count, hjust = 0.5, vjust = 0.5),size = 7) +
    scale_fill_manual(values = color_scheme) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 4),
                       labels = comma) +
    theme(legend.position = "none",
          axis.title = element_text(size = 17),
          axis.text.y = element_blank(),
          axis.text = element_text(size = 18),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.title = element_text(hjust = 0.5, face = "bold"))
  
  length <- EU_kmean_result %>%
    ggplot(aes(x = as.factor(input),
               y = Length,
               fill = as.factor(input))) +
    stat_summary(fun = mean, #summarizing y
                 geom = "bar")+ #with bars
    scale_y_continuous(limits=c(0,850), breaks = scales::pretty_breaks(n = 4),labels = comma)+
    theme_bw()+
    labs(x = "",
         y = "Median Length",
         title = "Length") +
    
    geom_hline(yintercept = quantile(EU_kmean_result$Length, 0.666),
               color = "black", size = 0.8)+
    geom_text(aes(x=1.5,y=quantile(EU_kmean_result$Length, 0.666), label = "\"Above\" threshold"), vjust = -1, size= 4)+
    
    geom_hline(yintercept = quantile(EU_kmean_result$Length, 0.2),
               color = "black", size = 0.8, linetype="dashed")+
    geom_text(aes(x=1.5,y=quantile(EU_kmean_result$Length, 0.2), label = "\"Below\" threshold"), vjust = -1, size= 4)+
    
    theme(legend.position = "none")+
    scale_fill_manual(values = color_scheme) +
    theme(legend.position = "none",
          axis.title = element_text(size = 15),
          axis.text = element_text(size = 16),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.title = element_text(hjust = 0.5, face = "bold"))
  
  frequency <- EU_kmean_result %>%
    ggplot(aes(x = as.factor(input),
               y = Frequency,
               fill = as.factor(input))) +
    stat_summary(fun = mean, #summarizing y
                 geom = "bar")+ #with bars
    theme_bw()+
    labs(x = "",
         y = "Median Frequency",
         title = "Frequency") +
    
    geom_hline(yintercept = quantile(EU_kmean_result$Frequency, 0.666),
               color = "black", size = 0.8)+
    geom_text(aes(x=1.5,y=quantile(EU_kmean_result$Frequency, 0.666), label = "\"Above\" threshold"), vjust = -1, size= 4)+
    
    geom_hline(yintercept = quantile(EU_kmean_result$Frequency, 0.333),
               color = "black", size = 0.8, linetype="dashed")+
    geom_text(aes(x=1.5,y=quantile(EU_kmean_result$Frequency, 0.333), label = "\"Below\" threshold"), vjust = -1, size= 4)+
    
    theme(legend.position = "none")+
    scale_fill_manual(values = color_scheme) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 4),
                       labels = comma) +
    theme(legend.position = "none",
          axis.title = element_text(size = 15),
          axis.text = element_text(size = 16),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.title = element_text(hjust = 0.5, face = "bold"))
  
  recency <- EU_kmean_result %>%
    ggplot(aes(x = as.factor(input),
               y = Recency,
               fill = as.factor(input))) +
    stat_summary(fun = mean, #summarizing y
                 geom = "bar")+ #with bars
    theme_bw()+
    scale_y_continuous(limits=c(0,850), breaks = scales::pretty_breaks(n = 4),labels = comma)+
    labs(x = "",
         y = "Median Recency",
         title = "Recency") +
    
    geom_hline(yintercept = quantile(EU_kmean_result$Recency,0.666),
               color = "black", size = 0.8)+
    geom_text(aes(x=1.5,y=quantile(EU_kmean_result$Recency,0.666), label = "\"Above\" threshold"), vjust = -1, size= 4)+
    
    geom_hline(yintercept = quantile(EU_kmean_result$Recency,0.333),
               color = "black", size = 0.8, linetype="dashed")+
    geom_text(aes(x=1.5,y=quantile(EU_kmean_result$Recency,0.333), label = "\"Below\" threshold"), vjust = -1, size= 4)+
    
    scale_fill_manual(values = color_scheme) +
    theme(legend.position = "none",
          axis.title = element_text(size = 15),
          axis.text = element_text(size = 16),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.title = element_text(hjust = 0.5, face = "bold"))
  
  monetary <- EU_kmean_result %>%
    ggplot(aes(x = as.factor(input),
               y = Monetary,
               fill = as.factor(input))) +
    stat_summary(fun = mean, #summarizing y
                 geom = "bar")+ #with bars
    theme_bw()+
    labs(x = "",
         y = "Median Monetary",
         title = "Monetary") +
    
    geom_hline(yintercept = quantile(EU_kmean_result$Monetary,0.666),
               color = "black", size = 0.8)+
    geom_text(aes(x=1.5,y=quantile(EU_kmean_result$Monetary,0.666), label = "\"Above\" threshold"), vjust = -1, size= 4)+
    
    geom_hline(yintercept = quantile(EU_kmean_result$Monetary,0.333),
               color = "black", size = 0.8, linetype="dashed")+
    geom_text(aes(x=1.5,y=quantile(EU_kmean_result$Monetary,0.333), label = "\"Below\" threshold"), vjust = -1, size= 4)+
    
    scale_y_continuous(breaks = scales::pretty_breaks(n = 4),
                       labels = comma)+
    theme(legend.position = "none",
          axis.title = element_text(size = 15),
          axis.text = element_text(size = 16),
          plot.caption = element_text(size = 15),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.title = element_text(hjust = 0.5, face = "bold")) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 4),
                       labels = comma) +
    scale_fill_manual(values = color_scheme)
  
  #arrange grid
  grid.arrange(count, arrangeGrob(length, recency, frequency, monetary, nrow = 2, ncol = 2),
               heights=c(0.37,0.63), ncol=1,
               top = textGrob("LFRM Characteristics EU Market", gp = gpar(fontsize=20,font=2))
  )
}

cluster_viz(EU_kmean_result$Cluster_6)

#Export data
setwd("C:\\Users\\Admin\\Documents\\Data Got Talent\\Final round\\LFRM model\\Filter Bike By Region\\Filter bike\\Europe")

## Sales data _ with clusters appended
EU_sales_data_raw <- read_xlsx("C:\\Users\\Admin\\Documents\\Data Got Talent\\Final round\\All data in one sheet.xlsx", 
                               sheet = "All data") %>% as.data.frame() %>% 
  filter(SalesTerritoryGroup == "Europe" & ProductCategoryKey == 1)

EU_sales_data_raw$CustomerKey <- EU_sales_data_raw$CustomerKey %>% as.factor()
EU_sales_data_raw$OrderDate <- EU_sales_data_raw$OrderDate %>% ymd()

EU_clustered_sales_data <- EU_sales_data_raw %>%
  left_join(EU_kmean_result_export %>% dplyr::select(CustomerKey,Cluster_6), by = "CustomerKey")

export(EU_clustered_sales_data, "Sales_Clustered_EU_bike.xlsx")

## Customer database _ with clusters appended
EU_customer_data_raw <- read_xlsx("C:\\Users\\Admin\\Documents\\Data Got Talent\\Final round\\Raw_Huy V1.xlsx", sheet = "Customer") %>% as.data.frame()

EU_customer_data_raw$CustomerKey <- EU_customer_data_raw$CustomerKey %>% as.factor()

EU_clustered_customer_data <- EU_customer_data_raw %>%
  left_join(EU_kmean_result_export %>% dplyr::select(CustomerKey,Cluster_6), by = "CustomerKey") %>%
  left_join(EU_sales_data_raw %>%
              dplyr::select(CustomerKey, SalesTerritoryGroup, SalesTerritoryCountry)
  )

EU_clustered_customer_data <- EU_clustered_customer_data[complete.cases(EU_clustered_customer_data$Cluster_6),]

EU_clustered_customer_data %>% summary()

export(EU_clustered_customer_data, "Customer_Clustered_EU_bike.xlsx")

## Export characteristics of clusters _ EU
export(EU_kmean_result, "EU_Cluster Characteristics_bike.xlsx")
