# Loading the required libraries
library(rpart)
library(partykit) # Treeplots
library(MASS)
library(ElemStatLearn)
library(randomForest)
library(gbm)
library(caret)

set.seed(123)
# Checking the summary of data
data("prostate")
str(prostate)
summary(prostate)

prostate$gleason = ifelse(prostate$gleason == 6, 0,1)
train = subset(prostate, train = TRUE)[,1:9]
test = subset(prostate, train = FALSE)[,1:9]

rf = randomForest(lpsa~., data = train)
rf
plot(rf)
which.min(rf$mse)

set.seed(123)
rf2 = randomForest(lpsa~., data = train, ntree = 34)
rf2

varImpPlot(rf2)

pred = predict(rf2, newdata = test[,-9])

res = pred - test$lpsa
mean(res^2)
