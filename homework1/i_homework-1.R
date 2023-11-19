library(readr)
library(dlookr)
library(DescTools)
library(ROCR)

# STEP 1: DATA – IMPORT AND DATA CHECK
setwd("/Users/severinclauss/it-stuff/bfh-sbd2/week5")
churn_data <- read_csv("Telco-Customer-Churn_CLEANED.csv")
data <- churn_data # We make a copy from the original dataset and work on the copy

# Checking dimensions and structure of the data
dim(data)
str(data)
head(data)
library(caret)

# We check the summary of all variables included. Here might be able to identify some data quality issues.
summary(data)

# Examine if there are missing values
diagnose(data)

# Check how represented each class is
table(data$Churn)

# STEP 2: TRAIN A ML CLASSIFIER (LOGISTIC REGRESSION)
# Use a 70/30 split for train and test data.
set.seed(7) # Set random seed to make results reproducible
data$Churn <- as.factor(data$Churn)
div <- createDataPartition(y = data$Churn, p = 0.7, list = F) # split data for training and test

# Training Sample
data.train <- data[div,] # 70% here
PercTable(data.train$Churn) # check the distribution of the target variable in train data

# Test Sample
data.test <- data[-div,] # rest of the 30% data goes here
PercTable(data.test$Churn) # check the distribution of the target variable in test data

# Train a model explaining Churn with all available variables as inputs (.) (i.e. Xs)
churn_model_lr1 <- glm(Churn ~ ., data=data.train,family=binomial())

# Print results of the model
summary(churn_model_lr1)

# STEP 3: PREDICT THE CHURN OF JACK SHEPHARD
# First, enter values for all predictors
new_customer_jack_shephard <- data.frame(
  Account_Length_days=170,
  Voicemail_Messages=14,
  Call_mins_day=300,
  Call_mins_eve=24,
  Call_mins_night=3,
  CustomerService_Calls=0,
  International_Calls=18,
  Customer_Age=46,
  Customer_Gender="male")

# Second, make the prediction on the basis of the data you entered
predict(churn_model_lr1, type='response', new_customer_jack_shephard)

## NEXT
# Make predictions on the test data
data.test$churn_score_lg <- predict(churn_model_lr1, type='response', data.test)

# Create the ROC Curve
churn_pred <- prediction(data.test$churn_score_lg, data.test$Churn)
churn_roc <- performance(churn_pred, "tpr", "fpr")
plot(churn_roc, lwd=1, colorize = TRUE, main = "Churn_model: ROC Curve for logistic classifier")
lines(x=c(0, 1), y=c(0, 1), col="black", lwd=1, lty=3)

# Create the precision/recall curve
churn_model_precision <- performance(churn_pred, measure = "prec", x.measure = "rec")
plot(churn_model_precision, main="Churn_model: Precision vs Recall")

## Create the confusion matrix
# Predict churn if probability is greater than 50%
data.test$churn_predicted_lg <- ifelse(data.test$churn_score_lg > 0.5, "yes", "no") # threshold is 0.5
data.test$churn_predicted_lg <- as.factor (data.test$churn_predicted_lg)
confusionMatrix(data=data.test$churn_predicted_lg, reference = data.test$Churn)

#Predictive utility (Area Under the Curve = AUC value)
churn_model_auc <- performance(churn_pred, measure = "auc")
cat("AUC: ",churn_model_auc@y.values[[1]]*100)



