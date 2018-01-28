# Clearing the work space
rm(list = ls()) 

# Loading the required libraries
library (ISLR)

# Going through the dataset
auto = Auto
str(auto)
View(auto)
summary(auto)
# No NAs in the dataset

# Running Linear regression
reg = lm(mpg ~ horsepower, data = auto)
reg
summary(reg)

# Yes there is a negative relationship
# between predictor and the response

# Check the correlation between the two variables
cor(auto$mpg,auto$horsepower)
# There is a strong negative correlation

dev.off()
plot(auto$horsepower, auto$mpg,col = "blue")
# Line of the best fit
abline(reg,col="red") 
# The linear regression is not a good fit
# between the relationship between 
# Horsepower and mpg

abline(v = 98,col="red")

# Computing the confidence and the prediction intervals
con = predict(reg,data.frame(horsepower = 98),interval = "confidence")
pre = predict(reg,data.frame(horsepower = 98),interval = "prediction")

# Drawing the confidence intervals --> green
abline(h = con[1,2],col = "green")
abline(h = con[1,3],col = "green")

# Drawing the prediction intervals --> pink
abline(h = pre[1,2],col = "pink")
abline(h = pre[1,3],col = "pink")

# Drawing the fit line of the intervals -->black
abline(h = pre[1,1],col = "black")

# Diagnostic plots of the linear regressor
plot(reg)