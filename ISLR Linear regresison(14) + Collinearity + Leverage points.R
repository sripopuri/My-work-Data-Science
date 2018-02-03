# Clearing my work space
rm(list = ls())

# Setting the required seed for reproducibility
set.seed(1)

# Synthesizing the data
x1 = runif(100)
x2 = 0.5*x1 + rnorm(100)/10
y = 2 + 2*x1 + 0.3*x2 + rnorm(100)

# creating a linear model
reg1 = lm(y~x1+x2)
summary(reg1)
# Call:
#   lm(formula = y ~ x1 + x2)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -2.8311 -0.7273 -0.0537  0.6338  2.3359 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   2.1305     0.2319   9.188 7.61e-15 ***
#   x1            1.4396     0.7212   1.996   0.0487 *  
#   x2            1.0097     1.1337   0.891   0.3754    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 1.056 on 97 degrees of freedom
# Multiple R-squared:  0.2088,	Adjusted R-squared:  0.1925 
# F-statistic:  12.8 on 2 and 97 DF,  p-value: 1.164e-05

# The regression coefficients are 1.4 and 1 as opposed to
# 2 and 0.3 mentioned in the above equations to synthesize the data
# x1 and x2 are not quite that significant, comparitively
# x1 is slighlty significant compared to x2, but 
# we can reject the estimate of x1 since its p value 
# is close to 0.05, the adjusted R2 is quite poor for this model
# the null hypothesis cannot be rejected

cor(x1,x2)
# 0.8351212 --> High correlation

# Scatter plot
plot(x1,x2)

# Regression using x1 alone
reg2 = lm(y~x1)
summary(reg2)

# Call:
#   lm(formula = y ~ x1)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -2.89495 -0.66874 -0.07785  0.59221  2.45560 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   2.1124     0.2307   9.155 8.27e-15 ***
#   x1            1.9759     0.3963   4.986 2.66e-06 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 1.055 on 98 degrees of freedom
# Multiple R-squared:  0.2024,	Adjusted R-squared:  0.1942 
# F-statistic: 24.86 on 1 and 98 DF,  p-value: 2.661e-06

# The null hypothesis can be rejected here

# Regression using x2 alone
reg3 = lm(y~x2)
summary(reg3)

# Call:
#   lm(formula = y ~ x2)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -2.62687 -0.75156 -0.03598  0.72383  2.44890 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   2.3899     0.1949   12.26  < 2e-16 ***
#   x2            2.8996     0.6330    4.58 1.37e-05 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 1.072 on 98 degrees of freedom
# Multiple R-squared:  0.1763,	Adjusted R-squared:  0.1679 
# F-statistic: 20.98 on 1 and 98 DF,  p-value: 1.366e-05

# The null hypothesis can be rejected here as well


x1 = c(x1, 0.1)
x2 = c(x2, 0.8)
y = c(y, 6)

#Leverage point here
reg4 = lm(y~x1+x2)
summary(reg4)
plot(reg4)

# Outlier here
reg5 = lm(y~x1)
summary(reg5)
plot(reg5)

#Leverage point here
reg6 = lm(y~x2)
summary(reg6)
plot(reg6)