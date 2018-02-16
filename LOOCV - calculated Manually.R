# Clearing up my workspace
rm(list = ls())

# Loading required libraries
library(ISLR)
library(boot)
library(caret)

# Getting the required data
data = Weekly
sum(is.na(data))
colnames(data)
str(data)

# Fitting logistic regression models
lr1 = glm(Direction~Lag1 + Lag2, data, family = "binomial")
summary(lr1)

# Without the fist observation
lr2 = glm(Direction~Lag1 + Lag2, data[-1,], family = "binomial")
summary(lr2)

prediction = predict(lr2,data[1,], type = "response")
prediction
# 0.5713923
# Which is greater than 0.5 hence it is up
# But the first observation has direction: down
# So it is misclassified by the model

# Calculating LOOCV manually
n = nrow(data)
predictions = vector(mode = "double")
for(i in 1:n){
 logistic = glm(Direction~Lag1 + Lag2, data[-i,], family = "binomial")
 predictions[i] = predict(logistic,data[i,], type = "response")
 predictions[i] = ifelse(predictions[i] > 0.5,1,0)
}

# Taking the actual classes from the data
actual = ifelse(data$Direction == "Up",1,0)

# Calculating the test error rate
confusionMatrix(actual,predictions)
# 0.45
# Hence the test error rate is 45%