# Clearing the work space
rm(list = ls()) 

# Loading the required libraries
library (ISLR)

# Going through the dataset
car = Carseats
str(car)
# 'data.frame':	400 obs. of  11 variables:
#   $ Sales      : num  9.5 11.22 10.06 7.4 4.15 ...
# $ CompPrice  : num  138 111 113 117 141 124 115 136 132 132 ...
# $ Income     : num  73 48 35 100 64 113 105 81 110 113 ...
# $ Advertising: num  11 16 10 4 3 13 0 15 0 0 ...
# $ Population : num  276 260 269 466 340 501 45 425 108 131 ...
# $ Price      : num  120 83 80 97 128 72 108 120 124 124 ...
# $ ShelveLoc  : Factor w/ 3 levels "Bad","Good","Medium": 1 2 3 3 1 1 3 2 3 3 ...
# $ Age        : num  42 65 59 55 38 78 71 67 76 76 ...
# $ Education  : num  17 10 12 14 13 16 15 10 10 17 ...
# $ Urban      : Factor w/ 2 levels "No","Yes": 2 2 2 2 2 1 2 2 1 1 ...
# $ US         : Factor w/ 2 levels "No","Yes": 2 2 2 2 1 2 1 2 1 2 ...

summary(car)
# No NAs in the dataset
# Sales          CompPrice       Income        Advertising       Population   
# Min.   : 0.000   Min.   : 77   Min.   : 21.00   Min.   : 0.000   Min.   : 10.0  
# 1st Qu.: 5.390   1st Qu.:115   1st Qu.: 42.75   1st Qu.: 0.000   1st Qu.:139.0  
# Median : 7.490   Median :125   Median : 69.00   Median : 5.000   Median :272.0  
# Mean   : 7.496   Mean   :125   Mean   : 68.66   Mean   : 6.635   Mean   :264.8  
# 3rd Qu.: 9.320   3rd Qu.:135   3rd Qu.: 91.00   3rd Qu.:12.000   3rd Qu.:398.5  
# Max.   :16.270   Max.   :175   Max.   :120.00   Max.   :29.000   Max.   :509.0  
# Price        ShelveLoc        Age          Education    Urban       US     
# Min.   : 24.0   Bad   : 96   Min.   :25.00   Min.   :10.0   No :118   No :142  
# 1st Qu.:100.0   Good  : 85   1st Qu.:39.75   1st Qu.:12.0   Yes:282   Yes:258  
# Median :117.0   Medium:219   Median :54.50   Median :14.0                      
# Mean   :115.8                Mean   :53.32   Mean   :13.9                      
# 3rd Qu.:131.0                3rd Qu.:66.00   3rd Qu.:16.0                      
# Max.   :191.0                Max.   :80.00   Max.   :18.0

# Creating a linear model
reg = lm(Sales ~ Price + Urban + US, data = car)
summary(reg)
# Call:
#   lm(formula = Sales ~ Price + Urban + US, data = car)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -6.9206 -1.6220 -0.0564  1.5786  7.0581 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 13.043469   0.651012  20.036  < 2e-16 ***
#   Price       -0.054459   0.005242 -10.389  < 2e-16 ***
#   UrbanYes    -0.021916   0.271650  -0.081    0.936    
# USYes        1.200573   0.259042   4.635 4.86e-06 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 2.472 on 396 degrees of freedom
# Multiple R-squared:  0.2393,	Adjusted R-squared:  0.2335 
# F-statistic: 41.52 on 3 and 396 DF,  p-value: < 2.2e-16

# Firstly this model is not that good enough since the Rsquared
# value is very low. Also Two of thepredictors used in the model
# are qualitative.
# Hence the interpretation goes this way...
# For every 1000$ increase in the price of the car seat, there is
# a drop in the sales by 54units, keeping other predictors constant

# The coefficients of the Urban says that, the sales in the 
# urban areas is less than rural areas by 21units keeping other 
# predictors constant. Same is the case with the US variable.
# Sales in the US are 1200units more compared to the Non-US
# regions keeping other predictors constant

# We can reject the null hypothesis for the Price and the
# US predictor.


reg_smaller = lm(Sales ~ Price + US, data = car)
summary(reg_smaller)
# Call:
#   lm(formula = Sales ~ Price + US, data = car)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -6.9269 -1.6286 -0.0574  1.5766  7.0515 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 13.03079    0.63098  20.652  < 2e-16 ***
#   Price       -0.05448    0.00523 -10.416  < 2e-16 ***
#   USYes        1.19964    0.25846   4.641 4.71e-06 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 2.469 on 397 degrees of freedom
# Multiple R-squared:  0.2393,	Adjusted R-squared:  0.2354 
# F-statistic: 62.43 on 2 and 397 DF,  p-value: < 2.2e-16


# The second model is slightly better model than the previous 
# one considering the R squared as the deciding metric

# Confidenceintervals of the coefficients
# confint(reg_smaller)
#                   2.5 %      97.5 %
#   (Intercept) 11.79032020 14.27126531
# Price       -0.06475984 -0.04419543
# USYes        0.69151957  1.70776632
