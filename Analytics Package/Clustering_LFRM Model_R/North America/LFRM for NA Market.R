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
NA_sales_data <- read_xlsx("C:\\Users\\Admin\\Documents\\Data Got Talent\\Final round\\All data in one sheet.xlsx", sheet = "All data") %>% 
  as.data.frame() %>%
  filter(SalesTerritoryGroup == "North America" & ProductCategoryKey == 1)

NA_sales_data %>% names()
NA_sales_data %>% summary()

## Extract needed data
NA_sales_data <- NA_sales_data %>% dplyr::select(CustomerKey,OrderDate,SalesAmount, Order)

#Deal with the data
NA_sales_data$CustomerKey <- NA_sales_data$CustomerKey %>% as.factor()

NA_sales_data$OrderDate <- NA_sales_data$OrderDate %>% ymd()

## ---Data preparation - FRM model
(analysis_date <- max(NA_sales_data$OrderDate)) #Analysis_date is the final Order Date in the dataset 2020-01-28

NA_FRM_data <- NA_sales_data %>%
  group_by(CustomerKey) %>%
  summarise(
    Length = as.numeric(analysis_date - min(OrderDate)),
    Frequency = sum(Order),
    Recency = as.numeric(analysis_date - max(OrderDate)),
    Monetary = sum(SalesAmount))

#Check for duplicates
NA_FRM_data %>% filter(duplicated(NA_FRM_data$CustomerKey)) #A tibble: 0 x 5 means that the data have 0 row and 5 cols, meaning no duplicated data

#Let's take a glimpse on the FRM data
NA_FRM_data
NA_FRM_data %>% summary()
dim(NA_FRM_data) #The data has 2752 rows representing 2752 customers, and 4 columns)

## ---Preprocessing data for k-kmean analysis
NA_kmean_scaled_data <- NA_FRM_data[,2:5] %>% scale()

#find the best number of cluster
fviz_nbclust(NA_kmean_scaled_data, kmeans, method = "wss") # > k = 6 clusters

#run the analysis
NA_kmean6 <- kmeans(NA_kmean_scaled_data, centers = 6, iter.max = 100, nstart = 100)

fviz_cluster(NA_kmean6, data = NA_kmean_scaled_data)

#appending clustering result to cleaned data
NA_kmean_result_export <- NA_FRM_data %>% 
  as.data.frame() %>%
  dplyr::mutate(Cluster_6 = NA_kmean6$cluster)

#Preparing data for final output
NA_kmean_result <- NA_kmean_result_export %>%
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

for(i in 1:dim(NA_kmean_result)[1]){
  
  if(NA_kmean_result$Length[i] <= quantile(NA_kmean_result$Length, 1) &
     NA_kmean_result$Length[i] > quantile(NA_kmean_result$Length, 0.666)){
    NA_kmean_result$Length.cmt[i] <- "La" #Length above avg
  } else {
    if(NA_kmean_result$Length[i] <= quantile(NA_kmean_result$Length, 0.666) &
       NA_kmean_result$Length[i] > quantile(NA_kmean_result$Length, 0.333)){
      NA_kmean_result$Length.cmt[i] <- "Lm" #Length middle avg 
    } else {
      NA_kmean_result$Length.cmt[i] <- "Lb" #Length below average
    }}
  
  if(NA_kmean_result$Monetary[i] <= quantile(NA_kmean_result$Monetary, 1) &
     NA_kmean_result$Monetary[i] > quantile(NA_kmean_result$Monetary, 0.666)){
    NA_kmean_result$Monetary.cmt[i] <- "Ma" #Monetary above avg
  } else {
    if(NA_kmean_result$Monetary[i] <= quantile(NA_kmean_result$Monetary, 0.666) &
       NA_kmean_result$Monetary[i] > quantile(NA_kmean_result$Monetary, 0.333)){
      NA_kmean_result$Monetary.cmt[i] <- "Mm" #Monetary middle avg 
    } else {
      NA_kmean_result$Monetary.cmt[i] <- "Mb" #Monetary below average
    }}
  
  if(NA_kmean_result$Recency[i] <= quantile(NA_kmean_result$Recency, 1)&
     NA_kmean_result$Recency[i] > quantile(NA_kmean_result$Recency, 0.666)) {
    NA_kmean_result$Recency.cmt[i] <- "Ra" #Recency above average
  } else {
    if(NA_kmean_result$Recency[i] <= quantile(NA_kmean_result$Recency, 0.666) &
       NA_kmean_result$Recency[i] > quantile(NA_kmean_result$Recency, 0.333)) {
      NA_kmean_result$Recency.cmt[i] <- "Rm" #Recency middle average
    } else {
      NA_kmean_result$Recency.cmt[i] <- "Rb" #Recency below average
    }}
  
  if(NA_kmean_result$Frequency[i] <= quantile(NA_kmean_result$Frequency, 1) &
     NA_kmean_result$Frequency[i] > quantile(NA_kmean_result$Frequency, 0.666)){
    NA_kmean_result$Frequency.cmt[i] <- "Fa" #Frequency above avg
  } else {
    if(NA_kmean_result$Frequency[i] <= quantile(NA_kmean_result$Frequency, 0.666) &
       NA_kmean_result$Frequency[i] > quantile(NA_kmean_result$Frequency, 0.333)){
      NA_kmean_result$Frequency.cmt[i] <- "Fm" #Frequency middle avg  
    } else {
      NA_kmean_result$Frequency.cmt[i] <- "Fb" #Frequency below average
    }}
  
  NA_kmean_result$Final.cmt[i] <- paste(NA_kmean_result$Length.cmt[i],
                                        NA_kmean_result$Frequency.cmt[i],
                                        NA_kmean_result$Recency.cmt[i],
                                        NA_kmean_result$Monetary.cmt[i]) #Summarise the attributes of each cluster group
}

## Visualization
color_scheme <- c('#e6ab02','#d95f02','#e7298a','#66a61e','#1b9e77','#1C6DD0','#30475E','#7570b3')

cluster_viz <- function(input){
  
  count <- NA_kmean_result %>%
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
  
  length <- NA_kmean_result %>%
    ggplot(aes(x = as.factor(input),
               y = Length,
               fill = as.factor(input))) +
    stat_summary(fun = mean, #summarizing y
                 geom = "bar")+ #with bars
    scale_y_continuous(limits=c(0,950), breaks = scales::pretty_breaks(n = 4),labels = comma)+
    theme_bw()+
    labs(x = "",
         y = "Median Length",
         title = "Length") +
    
    geom_hline(yintercept = quantile(NA_kmean_result$Length, 0.666),
               color = "black", size = 0.8)+
    geom_text(aes(x=1.5,y=quantile(NA_kmean_result$Length, 0.666), label = "\"Above\" threshold"), vjust = -1, size= 4)+
    
    geom_hline(yintercept = quantile(NA_kmean_result$Length, 0.333),
               color = "black", size = 0.8, linetype="dashed")+
    geom_text(aes(x=1.5,y=quantile(NA_kmean_result$Length, 0.333), label = "\"Below\" threshold"), vjust = -1, size= 4)+
    
    theme(legend.position = "none")+
    scale_fill_manual(values = color_scheme) +
    theme(legend.position = "none",
          axis.title = element_text(size = 15),
          axis.text = element_text(size = 16),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.title = element_text(hjust = 0.5, face = "bold"))
  
  frequency <- NA_kmean_result %>%
    ggplot(aes(x = as.factor(input),
               y = Frequency,
               fill = as.factor(input))) +
    stat_summary(fun = mean, #summarizing y
                 geom = "bar")+ #with bars
    theme_bw()+
    labs(x = "",
         y = "Median Frequency",
         title = "Frequency") +
    
    geom_hline(yintercept = quantile(NA_kmean_result$Frequency, 0.666),
               color = "black", size = 0.8)+
    geom_text(aes(x=1.5,y=quantile(NA_kmean_result$Frequency, 0.666), label = "\"Above\" threshold"), vjust = -1, size= 4)+
    
    geom_hline(yintercept = quantile(NA_kmean_result$Frequency, 0.3),
               color = "black", size = 0.8, linetype="dashed")+
    geom_text(aes(x=1.5,y=quantile(NA_kmean_result$Frequency, 0.3), label = "\"Below\" threshold"), vjust = -1, size= 4)+
    
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
  
  recency <- NA_kmean_result %>%
    ggplot(aes(x = as.factor(input),
               y = Recency,
               fill = as.factor(input))) +
    stat_summary(fun = mean, #summarizing y
                 geom = "bar")+ #with bars
    theme_bw()+
    scale_y_continuous(limits=c(0,950), breaks = scales::pretty_breaks(n = 4),labels = comma)+
    labs(x = "",
         y = "Median Recency",
         title = "Recency") +
    
    geom_hline(yintercept = quantile(NA_kmean_result$Recency,0.666),
               color = "black", size = 0.8)+
    geom_text(aes(x=1.5,y=quantile(NA_kmean_result$Recency,0.666), label = "\"Above\" threshold"), vjust = -1, size= 4)+
    
    geom_hline(yintercept = quantile(NA_kmean_result$Recency,0.333),
               color = "black", size = 0.8, linetype="dashed")+
    geom_text(aes(x=1.5,y=quantile(NA_kmean_result$Recency,0.333)-150, label = "\"Below\" threshold"), vjust = -1, size= 4)+
    
    scale_fill_manual(values = color_scheme) +
    theme(legend.position = "none",
          axis.title = element_text(size = 15),
          axis.text = element_text(size = 16),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.title = element_text(hjust = 0.5, face = "bold"))
  
  monetary <- NA_kmean_result %>%
    ggplot(aes(x = as.factor(input),
               y = Monetary,
               fill = as.factor(input))) +
    stat_summary(fun = mean, #summarizing y
                 geom = "bar")+ #with bars
    theme_bw()+
    labs(x = "",
         y = "Median Monetary",
         title = "Monetary") +
    
    geom_hline(yintercept = quantile(NA_kmean_result$Monetary,0.666),
               color = "black", size = 0.8)+
    geom_text(aes(x=1.5,y=quantile(NA_kmean_result$Monetary,0.666), label = "\"Above\" threshold"), vjust = -1, size= 4)+
    
    geom_hline(yintercept = quantile(NA_kmean_result$Monetary,0.333),
               color = "black", size = 0.8, linetype="dashed")+
    geom_text(aes(x=1.5,y=quantile(NA_kmean_result$Monetary,0.333), label = "\"Below\" threshold"), vjust = -1, size= 4)+
    
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
               top = textGrob("LFRM Characteristics NA Market", gp = gpar(fontsize=20,font=2))
  )
}

cluster_viz(NA_kmean_result$Cluster_6)
#Export data
setwd("C:\\Users\\Admin\\Documents\\Data Got Talent\\Final round\\LFRM model\\Filter Bike By Region\\Filter bike\\North America")

## Sales data _ with clusters appended
NA_sales_data_raw <- read_xlsx("C:\\Users\\Admin\\Documents\\Data Got Talent\\Final round\\All data in one sheet.xlsx", 
                               sheet = "All data") %>% as.data.frame() %>% 
  filter(SalesTerritoryGroup == "North America" & ProductCategoryKey == 1)

NA_sales_data_raw$CustomerKey <- NA_sales_data_raw$CustomerKey %>% as.factor()
NA_sales_data_raw$OrderDate <- NA_sales_data_raw$OrderDate %>% ymd()

NA_clustered_sales_data <- NA_sales_data_raw %>%
  left_join(NA_kmean_result_export %>% dplyr::select(CustomerKey,Cluster_6), by = "CustomerKey")

export(NA_clustered_sales_data, "Sales_Clustered_NA_bike.xlsx")

## Customer database _ with clusters appended
NA_customer_data_raw <- read_xlsx("C:\\Users\\Admin\\Documents\\Data Got Talent\\Final round\\Raw_Huy V1.xlsx", sheet = "Customer") %>% as.data.frame()

NA_customer_data_raw$CustomerKey <- NA_customer_data_raw$CustomerKey %>% as.factor()

NA_clustered_customer_data <- NA_customer_data_raw %>%
  left_join(NA_kmean_result_export %>% dplyr::select(CustomerKey,Cluster_6), by = "CustomerKey") %>%
  left_join(NA_sales_data_raw %>%
              dplyr::select(CustomerKey, SalesTerritoryGroup, SalesTerritoryCountry)
  )

NA_clustered_customer_data <- NA_clustered_customer_data[complete.cases(NA_clustered_customer_data$Cluster_6),]

NA_clustered_customer_data %>% summary()

export(NA_clustered_customer_data, "Customer_Clustered_NA_bike.xlsx")

## Export characteristics of clusters _ NA
export(NA_kmean_result, "NA_Cluster Characteristics_bike.xlsx")
