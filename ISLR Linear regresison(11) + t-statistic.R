# Clearing my workspace
rm(list = ls())

# Synthesizing the required data
set.seed(1)
x = rnorm(100)
y = 2*x + rnorm(100)

# Performing the linear regression y~x
# with out an intercept in the model
reg = lm(y~x+0)
summary(reg)

# Call:
#   lm(formula = y ~ x + 0)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -1.9154 -0.6472 -0.1771  0.5056  2.3109 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# x   1.9939     0.1065   18.73   <2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.9586 on 99 degrees of freedom
# Multiple R-squared:  0.7798,	Adjusted R-squared:  0.7776 
# F-statistic: 350.7 on 1 and 99 DF,  p-value: < 2.2e-16

# Observations:
# Coeff is nearly 2, std.error of the estimate is 0.1065
# t value 18.73 and p value <2e-16 suggests the significance
# of x on y ---> Null Hypothesis can be rejected
# y = 2x +e

# Performing the linear regression x~y
# with out an intercept in the model
reg = lm(x~y+0)
summary(reg)

# Call:
#   lm(formula = x ~ y + 0)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -0.8699 -0.2368  0.1030  0.2858  0.8938 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# y  0.39111    0.02089   18.73   <2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.4246 on 99 degrees of freedom
# Multiple R-squared:  0.7798,	Adjusted R-squared:  0.7776 
# F-statistic: 350.7 on 1 and 99 DF,  p-value: < 2.2e-16

# Observations:
# Coeff is nearly 0.4, std.error of the estimate is 0.02089
# t value 18.73 and p value <2e-16 suggests the significance
# of y on x ---> Null Hypothesis can be rejected
# x = 0.4y +e

# We have got the same t-statistic in both the fits

# Tstatistic
sqrt(length(x)-1)*(x%*%y)/sqrt(sum(x^2)*sum(y^2) - (x %*% y)^2)

summary(lm(y~x))
# Call:
#   lm(formula = y ~ x)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -1.8768 -0.6138 -0.1395  0.5394  2.3462 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -0.03769    0.09699  -0.389    0.698    
# x            1.99894    0.10773  18.556   <2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.9628 on 98 degrees of freedom
# Multiple R-squared:  0.7784,	Adjusted R-squared:  0.7762 
# F-statistic: 344.3 on 1 and 98 DF,  p-value: < 2.2e-16


summary(lm(x~y))
# Call:
#   lm(formula = x ~ y)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.90848 -0.28101  0.06274  0.24570  0.85736 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.03880    0.04266    0.91    0.365    
# y            0.38942    0.02099   18.56   <2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.4249 on 98 degrees of freedom
# Multiple R-squared:  0.7784,	Adjusted R-squared:  0.7762 
# F-statistic: 344.3 on 1 and 98 DF,  p-value: < 2.2e-16

# We can seee that the t-statistics are same for both the models.