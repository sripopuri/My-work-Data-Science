# Clearing up my workspace
rm(list = ls())

# Loading required libraries
library(ISLR)
library(MASS)
library(boot)
library(caret)

# Getting the dataset
data = Boston
str(data)
sum(is.na(data))
# No NA's in the dataset

mucap = mean(data$medv)
mucap
# 22.53281
stderror = sd(data$medv)/sqrt(nrow(data))
stderror
# 0.4088611

set.seed(1601)
boot.fn = function(data_temp,index){
  mu = mean(data_temp[index])
}

boot(data$medv,boot.fn,R = 1000)
# Bootstrap Statistics :
#   original      bias    std. error
# t1* 22.53281 -0.01931324   0.4057724
# There is not much difference between
# the bootstrap estimate and the other

t.test(data$medv)
# 95 percent confidence interval:
#   21.72953 23.33608

# 95% confidence intervals using
# Bootstrap model are,
# 21.72127 23.34435

# Both the bootstrap confidence interval
# and t test confidence intervals are pretty close

# Calculating the median value of medv
medi = median(data$medv)
# 21.2

set.seed(1601)
boot.med = function(data_temp,index){
  mumed = median(data_temp[index])
}

boot(data$medv,boot.med,R = 1000)
# We got a std error of 0.377

tenth = quantile(data$medv,c(0.1))
# 12.75

set.seed(1601)
boot.tenth = function(data_temp,index){
  muten = quantile(data_temp[index],c(0.1))
}

boot(data$medv,boot.tenth,R = 1000)
# The tenth percentile value of the bootstrap
# is same as the tenth percentile on the
# actual data
# The standard error of the tenth percentile,
# is 0.4893. 
