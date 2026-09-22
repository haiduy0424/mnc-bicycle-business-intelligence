# Business Performance & Customer Insights Dashboard

A data analytics and business intelligence project focused on evaluating sales performance, customer behavior, regional market dynamics, and future sales outlook for a multinational bicycle retailer.

The project combines **Power BI visualization, customer segmentation, statistical analysis, and forecasting techniques** to transform historical business data into practical insights for strategic decision-making.

## 1. Project Objectives

The analysis was designed to answer several key business questions:

- How has overall sales performance changed over time?
- Which products and markets are driving business growth?
- How effective is the company's product portfolio strategy?
- How do customer characteristics and purchasing behaviors differ across regions?
- Which customer groups generate the highest business value?
- What can historical sales patterns indicate about future performance?

## 2. Key Areas of Analysis

### 2.1. Sales Performance
- Evaluation of historical revenue, profitability, sales growth, product contribution, and changes in product mix across 2017–2019.

- The analysis identifies Mountain Bikes and newly introduced Touring Bikes as major contributors to the strong sales expansion observed in 2019. 

### 2.2. Product Portfolio Analysis
- Assessment of the company's strategic shift from high-end bicycles toward more affordable product segments, including its impact on sales volume, revenue, and profitability across different bike categories.

### 2.3. Regional Market Analysis
Comparison of business performance across:

- North America
- Europe
- Australia

The analysis evaluates market size, sales growth, volatility, customer value, demographics, loyalty, and purchasing behavior.

### 2.4. Customer Segmentation

Customer behavior is analyzed using an extended LRFM framework:

- **Length** — duration of the customer relationship
- **Recency** — time since the latest purchase
- **Frequency** — number of purchases
- **Monetary** — total customer spending

These variables are used as inputs for **K-Means clustering** to identify customer segments with different levels of loyalty, engagement, and profitability.

### 2.5. Sales Forecasting

The project also develops forecasting models for 2020 under different business scenarios.

Methods include:

- **ARIMA** for forecasting Gears & Outfits sales
- **VAR models** for bike sales forecasting
- Residual and model diagnostic tests
- Scenario-based forecasting

The VAR models incorporate relationships between bicycle sales and related gear sales to improve the forecasting framework.

## 3. Key Business Insights

- Sales reached approximately $16.35 million in 2019, representing a significant expansion compared with previous years.
- Mountain Bikes became a major growth driver, while Touring Bikes developed into a promising new revenue stream.
- Australia demonstrated strong customer loyalty and high customer value.
- North America showed strong customer acquisition but comparatively weaker customer engagement and average customer value.
- European markets displayed consistent growth despite operating at a smaller scale.
- Customer segmentation revealed substantial differences in spending, loyalty, and purchase frequency across markets.
- Gears showed a positive relationship with several bicycle categories, suggesting potential cross-selling opportunities.

## 4. Tools & Techniques

`Power BI` · `Data Visualization` · `Business Intelligence` · `K-Means Clustering` · `LRFM Analysis` · `Regression Analysis` · `ARIMA` · `VAR` · `Sales Forecasting`

## 5. Repository Structure

```text
├── PowerBI Dashboard.pbix
├── Report.pdf
└── README.md
