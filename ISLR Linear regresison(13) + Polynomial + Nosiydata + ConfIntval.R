# Clearing my work space
rm(list = ls())

# Setting the required seed for reproducibility
set.seed(1)

# synthesizing the data
x = rnorm(100,0,1)
eps = rnorm(100,0,0.5)

# Generating y
y = -1 + 0.5*x + eps
length(y)
 # Length of y is 100, coefficients are -1 and 0.5

# creating the linear model
reg = lm(y~x)
summary(reg)
# Call:
#   lm(formula = y ~ x)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.93842 -0.30688 -0.06975  0.26970  1.17309 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -1.01885    0.04849 -21.010  < 2e-16 ***
#   x            0.49947    0.05386   9.273 4.58e-15 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.4814 on 98 degrees of freedom
# Multiple R-squared:  0.4674,	Adjusted R-squared:  0.4619 
# F-statistic: 85.99 on 1 and 98 DF,  p-value: 4.583e-15

# Beta0 is -1.01885 and Beta1 is 0.49947

plot(x,y, main = "Scatter plot between x and y", col = "blue")

# I observe a linear relation between x and y adn estimated
# coefficients are close to the coeffs in the equation
abline(reg, col = "red")
abline(-1,0.5, col = "green")
legend("topleft", c("Best fit line", "Actual line"), col = c("red", "green"), lty = c(1,1))

# Polynomial regression
reg_pol = lm(y~ x + I(x^2))
summary(reg_pol)
# Call:
#   lm(formula = y ~ x + I(x^2))
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.98252 -0.31270 -0.06441  0.29014  1.13500 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -0.97164    0.05883 -16.517  < 2e-16 ***
#   x            0.50858    0.05399   9.420  2.4e-15 ***
#   I(x^2)      -0.05946    0.04238  -1.403    0.164    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.479 on 97 degrees of freedom
# Multiple R-squared:  0.4779,	Adjusted R-squared:  0.4672 
# F-statistic:  44.4 on 2 and 97 DF,  p-value: 2.038e-14

# The x^2 estimator is not having a significant relationship on y
# This is evident by the p values, though there is a slight increase 
# in the Adjusted R2 value, we cannot say this is a better model


# Less noise data set
# Modifying the variance of the eps term
# with sd = 0.2, hence a variance of 0.04
eps  = rnorm(100,0,0.2)

# running the same regressions once again

# Call:
#   lm(formula = y ~ x)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.58282 -0.09646 -0.00907  0.12985  0.52831 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -0.99453    0.02094  -47.49   <2e-16 ***
#   x            0.50423    0.02326   21.68   <2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.2079 on 98 degrees of freedom
# Multiple R-squared:  0.8275,	Adjusted R-squared:  0.8257 
# F-statistic:   470 on 1 and 98 DF,  p-value: < 2.2e-16

# The value of the coeff is 0.5 and there is great increase 
# in the Adjusted R2 value and decrease in the RSE value
# In the plot as well it is visible that the data is less
# spread aroung the regressor


# Polynomial model is actually not as good as linear model
# this can be observed below, with the R2 and the RSE values
# and also that the polynomial estimator is not having any
# significance in pedicting the response
# Call:
#   lm(formula = y ~ x + I(x^2))
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.58548 -0.09905 -0.00717  0.13191  0.52908 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -0.991774   0.025655 -38.658   <2e-16 ***
#   x            0.504765   0.023546  21.437   <2e-16 ***
#   I(x^2)      -0.003467   0.018483  -0.188    0.852    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.2089 on 97 degrees of freedom
# Multiple R-squared:  0.8275,	Adjusted R-squared:  0.824 
# F-statistic: 232.7 on 2 and 97 DF,  p-value: < 2.2e-16

# Confidence intervals for less noise data
confint(reg)
confint(reg_pol)

# > confint(reg)
# 2.5 %     97.5 %
#   (Intercept) -1.0360826 -0.9529699
# x            0.4580754  0.5503914
# > confint(reg_pol)
# 2.5 %      97.5 %
#   (Intercept) -1.04269192 -0.94085538
# x            0.45803251  0.55149677
# I(x^2)      -0.04015175  0.03321708

# Confidence Intervals for original data
confint(reg)
confint(reg_pol)

# > confint(reg)
# 2.5 %     97.5 %
#   (Intercept) -1.070670 -0.8716647
# x            0.361636  0.5826779
# > confint(reg_pol)
# 2.5 %       97.5 %
#   (Intercept) -1.0150148 -0.776956413
# x            0.3774241  0.595910959
# I(x^2)      -0.1804586 -0.008947781

# For less nosiy data the confidence interval is smaller 
# than the original data set. In other words, when there 
# is more noise in the data, the confidence intervals keep increasing