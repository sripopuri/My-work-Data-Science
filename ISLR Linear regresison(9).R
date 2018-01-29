# Clearing the work space
rm(list = ls()) 

# Loading the required libraries
library (ISLR)

# Going through the dataset
auto = Auto
str(auto)
summary(auto)
# No NAs in the dataset

# Scatter plot on all the variables in the auto dataset
# Excluding the 'name' variable from the plot
colnames(auto)
# name is the 9th column
auto_rev = auto[-9]
plot(auto_rev)

# Checking the correlations
cor(auto_rev)
corrplot::corrplot(cor(auto_rev),method = "ellipse",type = "lower",diag = F)

reg_all = lm(mpg ~ ., data = auto_rev)
reg_all
summary(reg_all)

# Not all the predictors have a 
# significant effect on the response
# Predictors: displacement, weight, year, origin
# are statistically significant

# The increase in every year usage of the car there is an 
# increase in the mpg of the car by 0.75miles per gallon

plot(reg_all)

