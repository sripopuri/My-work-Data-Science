###############################################################
# Title:        ps1.r
# Author:       Sri Harish Popuri
# Date:         2018-01-24
# Description:  Problem set 1 code file
###############################################################

rm(list=ls(all=TRUE))

## Import packages
library(data.table)

## Data import and validation
context1 <- fread('WAGE1.csv')
summary(context1)
str(context1)

# variable name   type    format     label      variable label
# wage            float   %8.2g                 average hourly earnings
# educ            byte    %8.0g                 years of education
# exper           byte    %8.0g                 years potential experience
# tenure          byte    %8.0g                 years with current employer
# nonwhite        byte    %8.0g                 =1 if nonwhite
# female          byte    %8.0g                 =1 if female
# married         byte    %8.0g                 =1 if married
# numdep          byte    %8.0g                 number of dependents
# smsa            byte    %8.0g                 =1 if live in SMSA
# northcen        byte    %8.0g                 =1 if live in north central U.S
# south           byte    %8.0g                 =1 if live in southern region
# west            byte    %8.0g                 =1 if live in western region
# construc        byte    %8.0g                 =1 if work in construc. indus.
# ndurman         byte    %8.0g                 =1 if in nondur. manuf. indus.
# trcommpu        byte    %8.0g                 =1 if in trans, commun, pub ut
# trade           byte    %8.0g                 =1 if in wholesale or retail
# services        byte    %8.0g                 =1 if in services indus.
# profserv        byte    %8.0g                 =1 if in prof. serv. indus.
# profocc         byte    %8.0g                 =1 if in profess. occupation
# clerocc         byte    %8.0g                 =1 if in clerical occupation
# servocc         byte    %8.0g                 =1 if in service occupation

# Generating a new variable on wage
context1$lwage <- log(context1$wage)

# Creating a linear models: Model 1,2 and 3
model1 <- lm(wage~educ, data = context1)
model2 <- lm(wage~educ + exper + tenure, data = context1)
model3 <- lm(lwage~educ + exper + tenure, data = context1)

## Summarize
summary(model1)

# Call:
#   lm(formula = wage ~ educ, data = context1)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -5.3707 -2.1578 -0.9854  1.1864 16.3975 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -0.93389    0.68769  -1.358    0.175    
# educ         0.54470    0.05346  10.189   <2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 3.392 on 524 degrees of freedom
# Multiple R-squared:  0.1654,	Adjusted R-squared:  0.1638 
# F-statistic: 103.8 on 1 and 524 DF,  p-value: < 2.2e-16


summary(model2)
# Call:
#   lm(formula = wage ~ educ + exper + tenure, data = context1)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -7.6498 -1.7708 -0.6407  1.2051 14.7201 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -2.91354    0.73172  -3.982 7.81e-05 ***
#   educ         0.60268    0.05148  11.708  < 2e-16 ***
#   exper        0.02252    0.01210   1.861   0.0633 .  
# tenure       0.17002    0.02173   7.825 2.83e-14 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 3.096 on 522 degrees of freedom
# Multiple R-squared:  0.3072,	Adjusted R-squared:  0.3032 
# F-statistic: 77.15 on 3 and 522 DF,  p-value: < 2.2e-16

summary(model3)
# Call:
#   lm(formula = lwage ~ educ + exper + tenure, data = context1)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -2.05911 -0.29563 -0.03302  0.28590  1.42657 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 0.282635   0.104331   2.709  0.00697 ** 
#   educ        0.092256   0.007340  12.569  < 2e-16 ***
#   exper       0.004137   0.001726   2.397  0.01687 *  
#   tenure      0.022112   0.003098   7.138 3.19e-12 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.4415 on 522 degrees of freedom
# Multiple R-squared:  0.3165,	Adjusted R-squared:  0.3125 
# F-statistic: 80.56 on 3 and 522 DF,  p-value: < 2.2e-16

## Interpretations

# a    For every 1year of increase in the education of the employee there
#      is an increase of 0.54 dollar(nearly) on average hourly wage.
# b    For every 1year of increase in the education of the employee there
#      is an increase of 0.60 dollar(nearly) on average hourly wage 
#      controlling experience and tenure of the employee
# c    For every 1year of increase in the experience of the employee there
#      is an increase of 0.02 dollar(nearly) on average hourly wage 
#      controlling education and tenure of the employee
# d    For every 1year of increase in the tenure of the employee there
#      is an increase of 0.17 dollar(nearly) on average hourly wage 
#      controlling education and experience of the employee
# e    The intercept of the model2 is negative. The intercepts in the
#      linear regression arenot for the interpretation, since 
#      the region where we are trying to interpret the data
#      is actually not having any data points.
# f    For every year increase in the education of the employee
#      the average hourly wage of the employee increases by 9.2%
#      for controlling the experience and the tenure of the employee
# g    For every year increase in the experience of the employee
#      the average hourly wage of the employee increases by 0.4%
#      for controlling the education and the tenure of the employee
# h    For every year increase in the tenure of the employee
#      the average hourly wage of the employee increases by 2.21%
#      for controlling the education and the experience of the employee
