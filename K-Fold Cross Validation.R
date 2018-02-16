# Clearing up my workspace
rm(list = ls())

# Loading required libraries
library(ISLR)
library(boot)

# Setting the seed
set.seed(1)
auto = Auto

# Sampling half of the observations
train = sample(392,196)
head(train)

# Fitting a linear regression model
lm.fit = lm(mpg~horsepower, data = auto, subset = train)
summary(lm.fit)

# Calculating the Test MSE
mean((auto[-train,1] - predict(lm.fit,auto[-train,]))^2)

# Building a Quadratic polynomial regression
lm.fit2 = lm(mpg~poly(horsepower,2), data = auto, subset = train)

# Calculating the test MSE
mean((auto[-train,1] - predict(lm.fit2,auto[-train,]))^2)

# Building a Quadratic polynomial regression
lm.fit3 = lm(mpg~poly(horsepower,3), data = auto, subset = train)

# Calculating the test MSE
mean((auto[-train,1] - predict(lm.fit3,auto[-train,]))^2)

# With different seed let us check the 
# results of the validation set approach
set.seed(2)

# Sampling the data again
train = sample(392,196)
head(train)

# Fitting a linear regression model
lm.fit = lm(mpg~horsepower, data = auto, subset = train)
summary(lm.fit)

# Calculating the Test MSE
mean((auto[-train,1] - predict(lm.fit,auto[-train,]))^2)

# Building a Quadratic polynomial regression
lm.fit2 = lm(mpg~poly(horsepower,2), data = auto, subset = train)

# Calculating the test MSE
mean((auto[-train,1] - predict(lm.fit2,auto[-train,]))^2)

# Building a Quadratic polynomial regression
lm.fit3 = lm(mpg~poly(horsepower,3), data = auto, subset = train)

# Calculating the test MSE
mean((auto[-train,1] - predict(lm.fit3,auto[-train,]))^2)


########### LOOCV - Leave One Out Cross-Validation

# Using the glm function
glm.fit = glm(mpg~horsepower, data=Auto)
summary(glm.fit)

cv.err =cv.glm(Auto ,glm.fit)

# Delta contains the result of the CV
cv.err$delta
# The first component is the raw cross-validation 
# estimate of prediction error. The second component 
# is the adjusted cross-validation estimate. 
# The adjustment is designed to compensate for the 
# bias introduced by not using leave-one-out cross-validation.
cv.err$K

# Calculating CV for polynomial regression
cv.error = rep(0,5)
for(i in 1:5){
  glm.fit = glm(mpg~poly(horsepower,i), data=Auto)
  cv.error[i] = cv.glm(Auto,glm.fit)$delta[1]
}

# Output of Cross validation error
cv.error


############ K fold cross validation
# Setting the seed
set.seed(17)
cv.error.10 = rep(0,10)
for(i in 1:10){
  glm.fit = glm(mpg~poly(horsepower,i), data=Auto)
  cv.error.10[i] = cv.glm(Auto,glm.fit, K = 10)$delta[1]
}

# Test errors for polynomial regressions respectively
cv.error.10