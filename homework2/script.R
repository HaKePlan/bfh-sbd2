library(readr)
library(dlookr)
library(dplyr)
library(patchwork)
library(ggplot2)
library(plotly)
library(car)
library(ggcorrplot)

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
  labs(title = 'Bitcoin price before 2017', subtitle = 'from 18.05.2016 to 31.12.2016', x = 'Bitcoin price in dollars', y = '') +
  theme(legend.position = "none", axis.text.y = element_blank())

bp_af17 <- ggplot(data = after_2017, aes(x = btc_coinbase)) +
  geom_boxplot(
    size = 1,
    outlier.shape = 1,
    outlier.color = "black",
    outlier.size  = 3,
    fill='skyblue') +
  labs(title = 'Bitcoin price after 2017', subtitle = 'from 01.01.2018 to 30.04.2018', x = 'Bitcoin price in dollars', y = '') +
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
# In the next step, we want to investigate the trend of the Bitcoin price.

# Using ggplot, create a line plot of the Bitcoin price (exchange: coinbase). Make sure to include a title and labels on the X and Y axis (Title = "The trend of the Bitcoin Price", x = "Date", y = "Bitcoin price in dollars").
ggplot(data = data, aes(y = btc_coinbase, x = Date)) +
  geom_line(color = "blue") +
  labs(title = "The trend of the Bitcoin Price", subtitle = 'from 18.05.2016 to 30.04.2018', x = "Date", y = "Bitcoin price in dollars") +
  theme(axis.text.x = element_text(angle=60, hjust=1))

# Using ggplot, plot a histogram of the Bitcoin price. Include the appropriate labels.
ggplot(data = data, aes(x = btc_coinbase)) +
  geom_histogram(fill = "skyblue", color = 'blue', bins = 20) +
  labs(title = "Distribution of the bitcoin prcie", subtitle = 'from 18.05.2016 to 30.04.2018', x = "Bitcoin price in dollars", y = "Counts") +
  theme(axis.text.x = element_text(angle=60, hjust=1))

# Using ggplot, plot a box plot of the Bitcoin price. Include the appropriate labels.
ggplot(data = data, aes(x = btc_coinbase)) +
  geom_boxplot(
    size = 1,
    outlier.shape = 1,
    outlier.color = "black",
    outlier.size  = 3,
    fill='skyblue') +
  labs(title = 'Distribution of the bitcoin prcie', subtitle = 'from 18.05.2016 to 30.04.2018', x = 'Bitcoin price in dollars', y = '') +
  theme(legend.position = "none", axis.text.y = element_blank()) +
  annotate("text", x = max(data$btc_coinbase), y = -0.3, label = paste("Mean =", round(mean(data$btc_coinbase), 2)), vjust = -1, hjust = 1, color = 'red')

# What can you tell about the price of the Bitcoin over the observed period?
# ...

# EXERCISE 4
# In the next step, we want to investigate the relationship between the price of Bitcoin and that of traditional assets.

# Create a graph to show the dependence between the Bitcoin price and the price of oil and plot a line (Hint: smooth method "lm"). What can you tell about the dependency between these two assets?
summary(lm(btc_coinbase~oil, data))
summary(lm(oil~btc_coinbase, data))
cor(data[-1])

ggplot(data = data, aes(x = oil, y = btc_coinbase)) +
  geom_point(color = 'skyblue') +
  geom_smooth(method = "lm", se = FALSE, color = 'blue') +
  labs(title = "Bitcoin Price vs. Oil Price",
       x = "Oil Price",
       y = "Bitcoin Price")

# Create a graph to show the dependence between the Bitcoin price and the price of gold.
# What can you tell about the dependency between these two assets?
viz <- ggplot(data = data, aes(x = gold, y = btc_coinbase)) +
  geom_point(color = 'skyblue') +
  geom_smooth(method = "lm", se = FALSE, color = 'blue') +
  labs(title = "Bitcoin Price vs. Gold Price",
       x = "Gold Price",
       y = "Bitcoin Price")

viz


# Using plotly, create a dynamic plot of the graph deﬁned in the previous step.
ggplotly(viz)

# Fit a multiple regression model where you try to predict the Bitcoin price (exchange = coinbase) by using the price of oil, gold and the SP500. Print the summary of the model and explain the results.
model <- lm(btc_coinbase~oil+gold+sp500, data)
summary(model)
coef(model)

# Plot the multiple regression model with the avPlots() function.
avPlots(model)

# EXERCISE 5)
# In the next step, we want to employ graphs to visualize the correlations identiﬁed.

# Deﬁne an object corr that would contain the correlations between all numeric data in the data set.
cor_matrix <- cor(data[-1])

# Using the ggcorrplot function, visualize the lower half of the correlation matrix also specifying the method = "circle".
ggcorrplot(cor_matrix, type = "lower", outline.col = "white", method = "circle", title = 'Correlation of the bitcoin dataset')

# Create a heatmap using plotly of the correlations that emerge between the numeric variables included in the dataset.
plot_ly(
  z = cor_matrix,
  x = colnames(cor_matrix),
  y = colnames(cor_matrix),
  colorscale = "Viridis",
  type = "heatmap"
) %>%
  layout(title = "Correlation Heatmap of the bitcoin dataset")

# EXERCISE 6)
# Having done the analysis - what can you conclude about the relationship between the Bitcoin price and traditional assets?