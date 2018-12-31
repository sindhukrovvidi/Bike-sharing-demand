#can write R code here and then click "Run" to run it on our platform
library(randomForest)
library(readr)
library(Metrics)
# The competition datafiles are in the directory ../input
setwd("C:/Users/SINDHU/Desktop/data science")

train=read.csv("train.csv")

train_factor<-train
train_factor$weather <- factor(train$weather)
train_factor$holiday <- factor(train$holiday)
train_factor$workingday <- factor(train$workingday)
train_factor$season <- factor(train$season)


#create time column by stripping out timestamp
train_factor$time <- substring(train$datetime,12,20)

train_factor$time <- factor(train_factor$time)

train_factor$day <- weekdays(as.Date(train_factor$datetime))
train_factor$day <- as.factor(train_factor$day)


#convert time and create $hour as integer to evaluate for daypart
train_factor$hour<- as.numeric(substr(train_factor$time,1,2))

#create daypart column, default to 4 to make things easier for ourselves
train_factor$daypart <- "4"

#4AM - 10AM = 1
train_factor$daypart[(train_factor$hour < 10) & (train_factor$hour > 3)] <- 1

#11AM - 3PM = 2
train_factor$daypart[(train_factor$hour < 16) & (train_factor$hour > 9)] <- 2

#4PM - 9PM = 3
train_factor$daypart[(train_factor$hour < 22) & (train_factor$hour > 15)] <- 3

#convert daypart to factor
train_factor$daypart <- as.factor(train_factor$daypart)

#convert hour back to factor
train_factor$hour <- as.factor(train_factor$hour)

library(caTools)
set.seed(99)
split <- sample.split(train$count, SplitRatio = 0.60)
training <- subset(train_factor, split == TRUE)

validation <- subset(train_factor, split == FALSE)

split <- sample.split(validation$count, SplitRatio = 0.50)
valid<- subset(validation, split == TRUE)
test <- subset(validation, split == FALSE)

fit <- randomForest(count ~season+holiday+workingday+weather+day+hour,method="anova", data=training)
plot(fit)

pred <- predict(fit, valid) 
plot(pred)
value<-rmse(valid$count,pred)
value

rf.pred.test <- predict(fit, test)
plot(rf.pred.test)
rmse(test$count,rf.pred.test)
