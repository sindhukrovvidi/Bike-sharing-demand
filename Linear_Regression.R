setwd("C:/Users/SINDHU/Desktop/data science")
train=read.csv("train.csv")
str(train)

train <- train[,-c(10,11)]

#Converting integer variables to factor variables
train$season <- as.factor(train$season)
train$holiday <- as.factor(train$holiday)
train$workingday <- as.factor(train$workingday)
train$weather <- as.factor(train$weather)
train$datetime <-as.POSIXct(train$datetime, format="%Y-%m-%d %H:%M:%S")

# Extract day from datetime value
train$day <-  strftime(train$datetime, '%u')
train$day <- as.factor(train$day)

# Extract hour from datetime value
train$hour <- substring(train$datetime, 12,13)
train$hour <- as.factor(train$hour)

# Removing datetime field 
train <- train[,-1]

library(caTools)
set.seed(123)
split <- sample.split(train$count, SplitRatio = 0.60)
training <- subset(train, split == TRUE)

validation <- subset(train, split == FALSE)

split <- sample.split(validation$count, SplitRatio = 0.50)
valid<- subset(validation, split == TRUE)
test <- subset(validation, split == FALSE)

str(training)
str(valid)
str(test)

# Applying Linear Regression model
lmBikeRent <- lm(count~., data = training)
lmBikeRent <- lm(count~season+holiday+workingday+weather+day+hour,data=training)

summary(lmBikeRent)

library(MASS)
lmBikeRentAIC<-stepAIC(lmBikeRent, direction="both")
summary(lmBikeRentAIC)



lm_predict_validation <- predict(lmBikeRentAIC, newdata = valid)

library(Metrics)
validaion_rmse<-rmse(valid$count,lm_predict_validation)
print("root-mean-square error between actual and predicted")

print(validaion_rmse)

# Let's check the summary of predicted count values
cat("\n")
print("summary of predicted count values")
summary(lm_predict_validation)

# summary of actual count values
print("summary of actual count values")
summary(valid$count)

# From above summary we saw negative values of predicted count.
# We don't want negative values as forecast for bike count. Replace all negative numbers with 1 
Output2Mod <- lm_predict_validation
Output2Mod[lm_predict_validation<=0] <-1

# Check again the summary of predicted count values
print("summary of predicted count values after replaced the negative values")
summary(Output2Mod)

# As we replaced the negative values, the rmse value got reduced
print("root-mean-square error value after replaced the negative values")
print(rmse(valid$count,Output2Mod))

validaion_rmse<-rmse(valid$count,Output2Mod)
print(validaion_rmse)

# Since we got negative predicted values, let's do log transformation and run regression model again
lmBikeRentLog <- lm(log(count)~., data = training)

# Now performs stepwise model selection on log model
lmBikeRentLogAIC <- stepAIC(lmBikeRentLog, direction="both")

lm_predict_validation_log <- predict(lmBikeRentLogAIC,newdata=valid)

# As the predicted values are in log format, use exponential(exp) to convert from log to non-log values
lm_predict_validation_nonlog <- exp(lm_predict_validation_log)

# Let's check the summary of predicted count values, it shows there are no negative values
print("summary of predicted count values after log transformation")
summary(lm_predict_validation_nonlog)

validaion_nonlog_rmse<-rmse(valid$count,lm_predict_validation_nonlog)
print("root-mean-square-log error value after log transformation")
print(validaion_nonlog_rmse)

test$count=0
str(test$count)
# Run model on test data
#lm_predict_validation_log <- predict(lmBikeRentLogAIC,newdata=valid)
lm_predict_test_log <- predict(lmBikeRentLogAIC,newdata=test)
str(test)
str(valid)
str(training)
lm_predict_test_nonlog <-exp(lm_predict_test_log)
final_df <- cbind(as.data.frame(lm_predict_test_log), test$hour)
colnames(final_df) <- c("count", "datetime")
final_df
