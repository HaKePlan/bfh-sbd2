library(readr)
library(dlookr)
# library(tidyverse)
library(dplyr)
library(patchwork)
library(ggplot2)

setwd('./homework2')
getwd()

dataImprot <- read_csv('./exchanges.csv')
data <- data.frame(dataImprot)



head(data)
summary(data)
overview(data)
diagnose(data)

str(data)



# EXERCISE 1)
# Using the object "data" (exchanges.csv), run basic descriptive statistics and answer the below questions:

# What period does that data cover?
data$Date <- as.Date(data$Date, format="%m/%d/%Y")

print(paste("The data covers the period from", min(data$Date), "to", max(data$Date)))


# What data types are included in the data set?
datatypes <- overview(data)
plot(datatypes)

# How would you evaluate the data quality?
diagnose(data)

# check for missing data
apply(data, 2, function(x) any(is.na(x)))

# show outliers and explain
boxplot(data[, sapply(data, is.numeric)])

# What are the mean and median of the numerical variables included?
summary(data)

# Which variable has the largest variation in the observed period? Which variable has the smallest variation in the observed period?
apply(data[, sapply(data, is.numeric)], 2, function(x) diff(range(x)))
# btc_coinbase largest
# usdeur smallest

# EXERCISE 2)
# Create a binary variable that will contain the value "Before" if the observation is before the year 2017; and "After" if the observation is after 2017. What is the mean price of the Bitcoin cryptocurrency (exchange: coinbase) for the period before and after 2017?

# filter data (dplyr package) and put it in two sets.
# We exclude 2017 since it seems not to be cosidert in the task description (before and after 2017)
before_2017 <- data |> filter(Date < as.Date('01.01.2017', format = '%d.%m.%Y'))
after_2017 <- data |> filter(Date > as.Date('31.12.2017', format = '%d.%m.%Y'))

# get the mean of before and after
bf17 <- mean(before_2017$btc_coinbase)
af17 <- mean(after_2017$btc_coinbase)

# create for both sets a boxplot
bp_bf17 <- ggplot(data = before_2017, aes(x = btc_coinbase)) +
  geom_boxplot(
    size = 1,
    outlier.shape = 1,
    outlier.color = "black",
    outlier.size  = 3,
    fill='skyblue') +
  labs(title = 'Bitcoin price before 2017', subtitle = 'from 18.05.2016 to 31.12.2016', x = 'Price coinbase', y = '') +
  theme(legend.position = "none", axis.text.y = element_blank())

bp_af17 <- ggplot(data = after_2017, aes(x = btc_coinbase)) +
  geom_boxplot(
    size = 1,
    outlier.shape = 1,
    outlier.color = "black",
    outlier.size  = 3,
    fill='skyblue') +
  labs(title = 'Bitcoin price after 2017', subtitle = 'from 01.01.2018 to 30.04.2018', x = 'Price coinbase', y = '') +
  theme(legend.position = "none", axis.text.y = element_blank())

# add a annotation to show the mean in the plot
result_bp_bf17 <- bp_bf17 + annotate("text", x = max(before_2017$btc_coinbase), y = -0.3, label = paste("Mean =", round(bf17, 2)), vjust = -1, hjust = 1, color = 'red')
result_bp_af17 <- bp_af17 + annotate("text", x = max(after_2017$btc_coinbase), y = -0.3, label = paste("Mean =", round(af17, 2)), vjust = -1, hjust = 1, color = 'red')

# use the patchwork library to print both plots on the same line
result_bp_bf17 + result_bp_af17



# Create a column titled "Diﬀerence" that will contain the diﬀerence between the Bitcoin price traded on the Coinbase and the Kraken exchange.
# On what day was the diﬀerence largest?
coinbase_kraken_diff <- data |>
  mutate(difference_coinbase_kraken = btc_coinbase - btc_kraken) |>
  arrange(desc(difference_coinbase_kraken)) |>
  slice_head(n = 1)

print(paste('The biggest difference (', coinbase_kraken_diff$difference_coinbase_kraken, ') on the', coinbase_kraken_diff$Date))

# EXERCISE 3
