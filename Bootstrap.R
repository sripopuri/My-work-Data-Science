# Clearing up my workspace
rm(list = ls())

# Loading required libraries
library(boot)
library(ISLR)

# Getting the data set
dataset = Portfolio
summary(dataset)
str(dataset)

# Creating a function to create a
# parameter of our interest
alpha.fn = function(data,index){
  X = data$X[index]
  Y = data$Y[index]
  return((var(Y)-cov(X,Y))/(var(X) + var(Y) - 2*cov(X,Y)))
}

alpha.fn(dataset,1:100)

# Recomputing alpha with generated
# data set from the original dataset
# with replacement
set.seed(1)
alpha.fn(Portfolio,sample(100,100,replace = T))

# Instead of repeating the code above
# number of time we can use the boot
# function below to produce such bootstrap
# 1000 samples and calculate our alpha estimate
boot(data = Portfolio,alpha.fn, R = 1000)


# Estimating the linear model
boot.fn = function(data,index){
  return(coef(lm(mpg~horsepower + I(horsepower^2), data = data,subset = index)))
}

boot.fn(Auto,1:392)
set.seed(1)
boot.fn(Auto,sample(392,392, replace = T))
boot.fn(Auto,sample(392,392, replace = T))

# Bootstrap
boot(Auto,boot.fn,1000)

# Comparing Bootstrap with summary statistics
summary (lm(mpg~horsepower + I(horsepower^2),data=Auto))$coef
