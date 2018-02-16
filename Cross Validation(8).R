# Clearing up my workspace
rm(list = ls())

# Loading required libraries
library(ISLR)
library(boot)
library(caret)

# Generating the data
set.seed(1)
x = rnorm(100)
y = x - 2*x^2 + rnorm(100)

# n = 100 which are observations
# p = 2 which are variables
# Model: y = x -(2*(x^2)) + noise

# scatter plot of x and y
plot(x,y, main = "Scatter plot of x and y")
# It is of parabolic form, which will be in the
# 3rd and 4th quandrant of the cordiante axes

data = data.frame(x,y)
error = matrix(0, nrow =2, ncol = 4)
colnames(error) = c('1st degree','2nd degree', '3rd degree','4th degree')
rownames(error) = c('1st seed', '2nd seed')
# Creating a for loop to create different
# polynomial models & seeds with out having to write
# the same code multiple times

for(p in 1:2){
  set.seed(p)
  for(i in 1:4){
    lr = glm(y~poly(x,i), data = data)
    error[p,i] = cv.glm(data,lr)$delta[1]
  }
}

error

# The results of both the seeds are same
# since LOOCV is an n-fold CV, and the results
# are averaged value by 'n'

# The smallest error model is the 2nd degree model
# and this is expected as the data generated is
# a second degree function of x

summary(lr)
# We see that only the first degree and the second
# degree coefficient estimates are statistically 
# significant compared to higher order estimates
# Yes these results are in line with the CB results