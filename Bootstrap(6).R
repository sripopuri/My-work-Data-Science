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
# The estimated standard errors for this model
# are income: 4.985e-06, balance: 2.274e-04

# Creating a boot function
# that returns the coefficients
boot.fn = function(data_temp,index){
  coef(glm(default~income + balance, 
                   data = data_temp[index,], 
                   family = "binomial"))
}

# Tetsing the boot.fn function
boot.fn(data,1:10000)
# Working good

boot(data,boot.fn, R = 1000)
# The standard errors that are obtained are
# income:4.853971e-06, balance:2.237182e-04
# Kind of similar standard errors from both 
# the procedures we adhered to