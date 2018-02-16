# Clearing up my workspace
rm(list = ls())

# Loading required libraries
library(ISLR)
library(boot)
library(caret)

# Getting the required data
data = Default
sum(is.na(data))
colnames(data)

set.seed(1601)
# Fitting a logistic regression
lr = glm(default~income + balance, data = data, family = "binomial")
summary(lr)

# Validation set approach
index = sample(nrow(data), nrow(data)/2)
train = data[index,]
validation = data[-index,]

# Fitting logistic on train data
lr_v = glm(default~income + balance, data = train, family = "binomial")
summary(lr_v)

# Predicting default status on validation set
predictions = predict(lr_v, validation, type = "response")
yesorno = ifelse(predictions > 0.5, "Yes", "No")

# Calculating the error rate
confusionMatrix(as.factor(yesorno), validation$default)
# Accuracy is 0.9738
# Error rate is 0.0262

# Repeating for a different seed
set.seed(1602)
index = sample(nrow(data), nrow(data)/2)
train = data[index,]
validation = data[-index,]
lr_v = glm(default~income + balance, data = train, family = "binomial")
predictions = predict(lr_v, validation, type = "response")
yesorno = ifelse(predictions > 0.5, "Yes", "No")
confusionMatrix(as.factor(yesorno), validation$default)
# Error rate is 0.024

# Repeating for a different seed
set.seed(1603)
index = sample(nrow(data), nrow(data)/2)
train = data[index,]
validation = data[-index,]
lr_v = glm(default~income + balance, data = train, family = "binomial")
predictions = predict(lr_v, validation, type = "response")
yesorno = ifelse(predictions > 0.5, "Yes", "No")
confusionMatrix(as.factor(yesorno), validation$default)
# Error rate is 0.0286

# Repeating for a different seed
set.seed(1604)
index = sample(nrow(data), nrow(data)/2)
train = data[index,]
validation = data[-index,]
lr_v = glm(default~income + balance, data = train, family = "binomial")
predictions = predict(lr_v, validation, type = "response")
yesorno = ifelse(predictions > 0.5, "Yes", "No")
confusionMatrix(as.factor(yesorno), validation$default)
# Error rate is 0.0274

# Hence the four different error rates are
# 0.0262,0.024,0.0286,0.0274
# These are quite varying from each kind of
# sampling to the other 0.024 to 0.0286

# creating a dummy variable for student with a 
# simple ifelse 1 or 0
train$dumstu = ifelse(train$student == "Yes",1,0)
validation$dumstu = ifelse(validation$student == "Yes",1,0)
lr_stud = glm(default~income + balance + dumstu, data = train, family = "binomial")
summary(lr_stud)

# The dumstud variable is not significant in the
# regression model, anyways we shall check the
# error rate as well
predictions = predict(lr_stud, validation, type = "response")
yesorno = ifelse(predictions > 0.5, "Yes", "No")
confusionMatrix(as.factor(yesorno), validation$default)

# Error rate is 0.0272
# As expected there is not much improvement in the
# test error rate of the model even after including
# student variable in the model