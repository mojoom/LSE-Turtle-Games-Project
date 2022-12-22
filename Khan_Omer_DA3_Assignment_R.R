## LSE Data Analytics Online Career Accelerator 

# DA301:  Advanced Analytics for Organisational Impact

###############################################################################

# Assignment template

## Scenario
## You are a data analyst working for Turtle Games, a game manufacturer and 
## retailer. They manufacture and sell their own products, along with sourcing
## and selling products manufactured by other companies. Their product range 
## includes books, board games, video games and toys. They have a global 
## customer base and have a business objective of improving overall sales 
##performance by utilising customer trends. 

## In particular, Turtle Games wants to understand:
## - how customers accumulate loyalty points (Week 1)
## - how useful are remuneration and spending scores data (Week 2)
## - can social data (e.g. customer reviews) be used in marketing 
##     campaigns (Week 3)
## - what is the impact on sales per product (Week 4)
## - the reliability of the data (e.g. normal distribution, Skewness, Kurtosis)
##     (Week 5)
## - if there is any possible relationship(s) in sales between North America,
##     Europe, and global sales (Week 6).

################################################################################

# Week 4 assignment: EDA using R

## The sales department of Turtle games prefers R to Python. As you can perform
## data analysis in R, you will explore and prepare the data set for analysis by
## utilising basic statistics and plots. Note that you will use this data set 
## in future modules as well and it is, therefore, strongly encouraged to first
## clean the data as per provided guidelines and then save a copy of the clean 
## data for future use.

# Instructions
# 1. Load and explore the data.
##  - Remove redundant columns (Ranking, Year, Genre, Publisher) by creating 
##      a subset of the data frame.
##  - Create a summary of the new data frame.
# 2. Create plots to review and determine insights into data set.
##  - Create scatterplots, histograms and boxplots to gain insights into
##      the Sales data.
##  - Note your observations and diagrams that could be used to provide
##      insights to the business.
# 3. Include your insights and observations.

###############################################################################

# 1. Load and explore the data

# Install and import Tidyverse.
install.packages("tidyverse")
library(tidyverse)

# Import the data set.
games <- read.csv(file.choose(), header=T)

# Print the data frame.
print(games)

# Create a new data frame from a subset of the sales data frame.
# Remove unnecessary columns. 
sales = subset(games, select = -c(Ranking, Year, Genre, Publisher))

# View the data frame.
View(sales)

# View the descriptive statistics.
summary(sales)

################################################################################

# 2. Review plots to determine insights into the data set.

## Scatterplots
# Create scatterplots.
library(ggplot2)
qplot(Product, Global_Sales, data=sales, geom=c('point', 'smooth'))

qplot(Product, Global_Sales, colour=RemappedPlatform, data=sales,
      geom=c('point', 'jitter'))

qplot(NA_Sales, Global_Sales, colour=RemappedPlatform, data=sales)

qplot(NA_Sales, Global_Sales, colour=RemappedPlatform, data=sales)

## Histograms
# Create histograms.
ggplot(sales, aes(x=Global_Sales, fill=Platform)) + geom_histogram()

ggplot(sales, aes(x=NA_Sales, fill=RemappedPlatform)) + 
  geom_histogram(binwidth=1)

# Interleaved histograms
ggplot(sales, aes(x=Global_Sales, color=RemappedPlatform)) +
  geom_histogram(binwidth=1, fill="white")

## Boxplots
# Create boxplot of platforms versus global sales
qplot(RemappedPlatform, Global_Sales, data=sales, geom='boxplot')

##bar chart

ggplot(data=sales, aes(x=Platform, y=Global_Sales)) + 
  geom_bar(stat="identity", width=0.5)

###############################################################################

# 3. Observations and insights

## Your observations and insights here ......
## Wii has the highest sales. All Playstation models combined have the highest
##volume



###############################################################################
###############################################################################


# Week 5 assignment: Cleaning and maniulating data using R

## Utilising R, you will explore, prepare and explain the normality of the data
## set based on plots, Skewness, Kurtosis, and a Shapiro-Wilk test. Note that
## you will use this data set in future modules as well and it is, therefore, 
## strongly encouraged to first clean the data as per provided guidelines and 
## then save a copy of the clean data for future use.

## Instructions
# 1. Load and explore the data.
##  - Continue to use the data frame that you prepared in the Week 4 assignment. 
##  - View the data frame to sense-check the data set.
##  - Determine the `min`, `max` and `mean` values of all the sales data.
##  - Create a summary of the data frame.
# 2. Determine the impact on sales per product_id.
##  - Use the group_by and aggregate functions to sum the values grouped by
##      product.
##  - Create a summary of the new data frame.
# 3. Create plots to review and determine insights into the data set.
##  - Create scatterplots, histograms, and boxplots to gain insights into 
##     the Sales data.
##  - Note your observations and diagrams that could be used to provide 
##     insights to the business.
# 4. Determine the normality of the data set.
##  - Create and explore Q-Q plots for all sales data.
##  - Perform a Shapiro-Wilk test on all the sales data.
##  - Determine the Skewness and Kurtosis of all the sales data.
##  - Determine if there is any correlation between the sales data columns.
# 5. Create plots to gain insights into the sales data.
##  - Compare all the sales data (columns) for any correlation(s).
##  - Add a trend line to the plots for ease of interpretation.
# 6. Include your insights and observations.

################################################################################

# 1. Load and explore the data
install.packages("skimr")
install.packages("DataExplorer")
library("dplyr")
library("skimr")
library("DataExplorer")
# View data frame created in Week 4.
View(sales)

# Check output: Determine the min, max, and mean values.
summarise(sales,
          mean_NA=mean(NA_Sales),
          mean_EU=mean(EU_Sales),
          mean_GL=mean(Global_Sales))

summarise(sales,
          min_NA=min(NA_Sales),
          min_EU=min(EU_Sales),
          min_GL=min(Global_Sales))  

summarise(sales,
          max_NA=max(NA_Sales),
          max_EU=max(EU_Sales),
          max_GL=max(Global_Sales)

# View the descriptive statistics.
summary(sales)

DataExplorer::create_report(sales)

###############################################################################

# 2. Determine the impact on sales per product_id.

## 2a) Use the group_by and aggregate functions.
# Group data based on Product and determine the sum per Product.
df_sales <- sales %>% group_by(Product) %>%
  summarise(sales_sum=sum(Global_Sales),
            .groups='drop')

df_salesNA <- sales %>% group_by(Product) %>%
  summarise(sales_sum=sum(NA_Sales),
            .groups='drop')

df_salesEU <- sales %>% group_by(Product) %>%
  summarise(sales_sum=sum(EU_Sales),
            .groups='drop')


# View the data frame.
View(df_sales)
View(df_salesNA)
View(df_salesEU)


# Explore the data frame.

summary(df_sales)
summary(df_salesEU)
summary(df_salesNA)

## 2b) Determine which plot is the best to compare game sales.
# Create scatterplots.
qplot(Product, sales_sum, data=df_sales,
      geom=c('point', 'jitter'))

qplot(Product, sales_sum, data=df_salesEU,
      geom=c('point', 'jitter'))

qplot(Product, sales_sum, data=df_salesNA,
      geom=c('point', 'jitter'))

# Create histograms.
ggplot(df_sales, aes(x=sales_sum)) + 
  geom_histogram(binwidth=1, fill="red")
# Create boxplots.
qplot(sales_sum, data=df_sales, geom='boxplot')

qplot(sales_sum, data=df_salesEU, geom='boxplot')

qplot(sales_sum, data=df_salesNA, geom='boxplot')



###############################################################################


# 3. Determine the normality of the data set.

## 3a) Create Q-Q Plots
# Create Q-Q Plots.

qqnorm(sales$Global_Sales,
       col='blue',
       xlab="z Value",
       ylab='Global Sales')

qqline(sales$Global_Sales,
       col='red',
       lwd=2) 

qqnorm(sales$EU_Sales,
       col='blue',
       xlab="z Value",
       ylab='Europe Sales')

qqline(sales$EU_Sales,
       col='red',
       lwd=2) 

qqnorm(sales$NA_Sales,
       col='blue',
       xlab="z Value",
       ylab='North America Sales')

qqline(sales$NA_Sales,
       col='red',
       lwd=2) 

## 3b) Perform Shapiro-Wilk test
# Install and import Moments.
install.packages("moments")
library("moments")


# Perform Shapiro-Wilk test.
shapiro.test(sales$Global_Sales)
shapiro.test(sales$EU_Sales)
shapiro.test(sales$NA_Sales)

## 3c) Determine Skewness and Kurtosis
# Skewness and Kurtosis.
skewness(sales$Global_Sales) 
kurtosis(sales$Global_Sales)

skewness(sales$EU_Sales) 
kurtosis(sales$EU_Sales)

skewness(sales$NA_Sales) 
kurtosis(sales$NA_Sales)

## 3d) Determine correlation
# Determine correlation.

cor(sales$Global_Sales, sales$NA_Sales)

cor(sales$Global_Sales, sales$EU_Sales)

cor(sales$EU_Sales, sales$NA_Sales)

cor(sales$Product, sales$Global_Sales)
###############################################################################

# 4. Plot the data
# Create plots to gain insights into data.
# Choose the type of plot you think best suits the data set and what you want 
# to investigate. Explain your answer in your report.

ggplot(data = sales, 
       mapping = aes(x=Product, y=Global_Sales, colour=Platform)) +
  geom_point() +
  theme_light()

# remove outliers
sales_outlier <- filter(sales, Global_Sales<40)

ggplot(data = sales_outlier,
       mapping = aes(x=Product, y=Global_Sales, colour=Platform)) +
  geom_point() +
  theme_light()

ggplot(data = sales, 
       mapping = aes(x=Platform, fill=Platform)) +
  geom_bar() +
  theme_minimal()


###############################################################################

# 5. Observations and insights
# Your observations and insights here...
## strong positive correlation between Global sales and NA Sales
## Strong positive correlation between global and EU sales (less than NA)
## significant positive correlation between Global and EU sales
## negative correlation between sales and product
## higher product id = lower sales. why?

###############################################################################
###############################################################################

# Week 6 assignment: Making recommendations to the business using R

## The sales department wants to better understand if there is any relationship
## between North America, Europe, and global sales. Therefore, you need to
## investigate any possible relationship(s) in the sales data by creating a 
## simple and multiple linear regression model. Based on the models and your
## previous analysis (Weeks 1-5), you will then provide recommendations to 
## Turtle Games based on:
##   - Do you have confidence in the models based on goodness of fit and
##        accuracy of predictions?
##   - What would your suggestions and recommendations be to the business?
##   - If needed, how would you improve the model(s)?
##   - Explain your answers.

# Instructions
# 1. Load and explore the data.
##  - Continue to use the data frame that you prepared in the Week 5 assignment. 
# 2. Create a simple linear regression model.
##  - Determine the correlation between the sales columns.
##  - View the output.
##  - Create plots to view the linear regression.
# 3. Create a multiple linear regression model
##  - Select only the numeric columns.
##  - Determine the correlation between the sales columns.
##  - View the output.
# 4. Predict global sales based on provided values. Compare your prediction to
#      the observed value(s).
##  - NA_Sales_sum of 34.02 and EU_Sales_sum of 23.80.
##  - NA_Sales_sum of 3.93 and EU_Sales_sum of 1.56.
##  - NA_Sales_sum of 2.73 and EU_Sales_sum of 0.65.
##  - NA_Sales_sum of 2.26 and EU_Sales_sum of 0.97.
##  - NA_Sales_sum of 22.08 and EU_Sales_sum of 0.52.
# 5. Include your insights and observations.

###############################################################################

# 1. Load and explor the data
# View data frame created in Week 5.
as_tibble(sales)

# Determine a summary of the data frame.
summary(sales)

###############################################################################

# 2. Create a simple linear regression model
## 2a) Determine the correlation between columns
# Create a linear regression model on the original data.

cor(sales$Global_Sales, sales$NA_Sales)
cor(sales$Global_Sales, sales$EU_Sales)


## 2b) Create a plot (simple linear regression)

#global vs NA 

d1 <- lm(Global_Sales~NA_Sales,
             data=sales)
d1

plot(sales$NA_Sales, sales$Global_Sales)+
abline(d1)

# Global Vs EU

d2 <- lm(Global_Sales~EU_Sales,
             data=sales)
d2

plot(sales$EU_Sales, sales$Global_Sales)
abline(d2)

#eu vs NA
d3 <- lm(NA_Sales~EU_Sales,
             data=sales)
summary(d3)

plot(sales$NA_Sales, sales$NA_Sales)+
  abline(d3)

###############################################################################

# 3. Create a multiple linear regression model
# Select only numeric columns from the original data frame.

sales_num <- select(sales, Product, NA_Sales, Global_Sales, EU_Sales)

as_tibble(sales_num)

# Correlation
cor(sales_num)

install.packages("psych")
library(psych)

corPlot(sales_num, cex=1)

###############################################################################

# 4. Predictions based on given values
# Compare with observed values for a number of records.

MLR_model <- lm(Global_Sales ~ NA_Sales + EU_Sales, data=sales)
summary(MLR_model)


pred_sales <- data.frame (NA_Sales = c(3.93, 2.73, 2.26, 22.08),
                          EU_Sales = c(1.56, 0.65, 0.97, 0.52)
)
pred_sales

predictTest = predict(MLR_model, newdata=pred_sales)

#print
predictTest 
###############################################################################

# 5. Observations and insights
# Your observations and insights here...
##


###############################################################################
###############################################################################




