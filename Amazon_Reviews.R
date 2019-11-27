# Learnings:
  # 1. In distinct(), .keep_all keeps all other variables in the output dataframe, in this case, just totalReviews
  # 2. Specify that the tables being read in have headers to avoid functions incorrectly reading the column headers
  
# Setting working directory

setwd('C:/Users/orlofpa/Desktop/Self_Learn/Amazon_Cell_Phone_Reviews/Amazon_Datasets')

getwd()


# Loading libraries

library(data.table)

library(dplyr)

library(ggplot2)

library(stringr)

library(readr)

library(tidyr)


# Loading all datasets

items <- fread('20190928-items.csv', header = TRUE)
  # Columns of immediate interest:  asin (unique ID), brand, title (name of phone), rating, number of reviews, prices

reviews <- fread('20190928-reviews.csv', header = TRUE)
  # Columns of immediate interest:  asin (unique ID), name (of reviewer), rating, date, verification of purchase, upvotes


# Viewing the datasets

str(items)
str(reviews)

head(items)
head(reviews)

View(items)
View(reviews)

summary(items)
summary(reviews)


# Objectives for EDA
  # 1. Identify what brand produces the best brand of phones
  # 2. Understand what brands provide the most bang for my buck

# Plan for EDA
  # 1. Drill datasets down to the data that I will be using for first analyses
  # 2. Combine both datasets into one on primary key "asin"
  # 3. Create a chart of average score, by brand
  # 4. Plot a chart of average scores divided by average price to find the best bang for the buck


# 1 and 2. Drilling down the dataset solely to what I need for the current exploration

#View(items[, c(1:2, 6, 8:9)]) - Viewed the result of column pickings prior to committing to a variable

items_simple <- items[, c(1:2, 6, 8:9)]

reviews_simple <- reviews[, c(1:5, 8)]

View(items_simple)
View(reviews_simple)

data_s <- left_join(items_simple, reviews_simple, by = 'asin')
data <- left_join(items, reviews, by = 'asin')

rm(items_simple, reviews_simple, items, reviews)
  # Removes unnecessary objects from my environment

gc()
  # Garbage collection - allows memory to be freed up

# Checking that the join process was completed successfully, all rows are present

str(data_s)
summary(data_s)

str(data)
summary(data)


View(data_s)
View(data)


# Comparing the number of verified reviews, by brand

data_s %>%
  select(brand, verified) %>%
  filter(verified == TRUE) %>%
  count(brand) %>%
  ggplot(aes(x = brand, y = n)) + 
    geom_col(fill = 'black') + 
    ggtitle('Number of Verified Phone Reviews on Amazon, by Brand') +
    ylab('Number of Verified Reviews') +
    xlab('Brand')


# Plotting the 5 most popular phones (assuming that the number of reviews can be used as a proxy variable)

count(distinct(data, title.x))
  # The dataset has 785 unique phones

data %>%
  select(title.x, totalReviews) %>%
  distinct(title.x, .keep_all = TRUE) %>%
    # In distinct(), .keep_all keeps all other variables in the output dataframe, in this case, just totalReviews
  arrange(desc(totalReviews)) %>%
  head(n = 5) %>%
  ggplot(aes(str_wrap(title.x, 15), y = totalReviews)) +
    # ggplot orders x-axis by alphabet, so I must add {, -totalReviews} within the x assingment
    geom_col(fill = 'black') +
    ggtitle('Five Most Popular Phones from the Dataset') +
    ylab('Number of Reviews') +
    xlab('Phone')
    #theme(axis.text.x = element_text(angle = -25, hjust = 0.05, vjust = 0.05)) - Angles and adjusts x-axis labels


# 3. Creating a chart of average score, by brand

data_s %>%
  distinct(asin, .keep_all = TRUE) %>%
  group_by(brand) %>%
    # The output data doesn't look any different, but now functions like summarize() will group results by the sepcified group
  select(asin, brand, rating.x, totalReviews) %>%
  #mutate(numRev_times_rev = rating.x * totalReviews) %>% - I used this before I simply put the multiple into the summarize 
  # function below
  summarize(avg_score = sum(rating.x * totalReviews) / sum(totalReviews)) %>%
  ggplot(aes(reorder(x = brand, - avg_score), y = avg_score)) +
    geom_col(fill = 'black') +
    ggtitle('Average Amazon Review Score, by Brand') +
    xlab('Brand') +
    ylab('Average Score')

# One thing to be aware of is that the top rated phones all have few enough reviews that these results very well may prove to 
# be statistically insignificant


# 4. Plot a chart of average scores divided by average price to find the best bang for the buck

data_s_x1 <- data_s

data_s_x1$prices <- as.numeric(gsub('\\$', '', data_s$prices))
  # data_s_x1$prices specifies gsub to be applied to just one column of the dataset, while keeping all other columns the same

data_s_x1 %>%
  distinct(asin, .keep_all = TRUE) %>%
  select(1:5) %>%
  filter(prices != '') %>%
  group_by(brand) %>%
  summarize(scorePrice_ratio = (sum(rating.x * totalReviews) / sum(totalReviews)) / 
            (sum(as.numeric(prices) * totalReviews) / sum(totalReviews))) %>%
              # The \\ escapes the $, as the $ is a built in functionality in R
  #gather('Type', 'Value', -brand) - This is code in case I want to have multiple variables for each x-value
    # This call brings all variables except for "brand" into one column, along with their corresponding values
  ggplot(aes(reorder(x = brand, -scorePrice_ratio), y = scorePrice_ratio)) +
    geom_col(fill = 'black') +
  ggtitle('Score to Price Ratio for Phone Brands on Amazon, by Brand') +
  xlab('Brand') +
  ylab('Score to Price Ratio')
