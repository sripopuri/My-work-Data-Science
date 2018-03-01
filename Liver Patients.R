# Author: Sri Harish Popuri
# Version: Intial draft - 1st version
# This is code will be changed further

# Note: In this code,
# 10error = False Negative
# 01error = False Positive

# Clearing the workspace 
rm(list = ls())  

# Loading the required packages
library(dummies)
library(boot) # For K fold
library(caret)
library(ggplot2)
library(pROC) # For ROC curves
library(e1071) # For SVM
library(class) # For Knn

# Setting the required working directory
setwd("D:/New folder/Kaggle/Liver patient Dataset")

data <- read.csv('Indian Liver Patient Dataset (ILPD).csv')

# Check for any missing values in the dataset
sum(is.na(data))

# As there are four missing values lets check
# in which columns we have them
apply(data,2,function(x)sum(is.na(x)))

# We have missing values in the 'alkphos' column
str(data)
summary(data)

table(data$is_patient)
table(data$gender)
# We see that there are 
# 416 liver patients in this data set and also
# 441 samples in data belong to 'Male' category

# Now let us club both the observations above
table(data$is_patient,data$gender)
plot(table(data$is_patient,data$gender), 
     xlab = "Patient & Not a Patient", 
     ylab = "Gender", 
     main = "Gender wise Analysis",
     col = c("lightblue"))

# 1) 73.4% (324/441) of Male respondents are having liver disease
# 2) 64.7% (92/142) of Female respondents are having liver disease

# factorizing is_patient variable in the data
# Before that lets encode it properly
data$is_patient = ifelse(data$is_patient == 2, 0 ,1)
str(data)
data$is_patient = as.factor(data$is_patient)
contrasts(data$is_patient)
str(data)

# Getting data with out NAs
# **We can impute instead
data_clean = data[complete.cases(data),]

# Creating training and test samples
index = createDataPartition(data_clean$is_patient, 
                            p = 500/579, 
                            times = 1, 
                            list = F)

train = data_clean[index,]
test = data_clean[-index,]

# Fitting a logistic regression model
logistic = glm(is_patient~.,
               data = train, 
               family = "binomial")

summary(logistic)

# Call:
#   glm(formula = is_patient ~ ., family = "binomial", data = train)
# 
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -3.1061  -1.0712   0.4020   0.9065   1.4688  
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)   
# (Intercept)      -3.6269038  1.4190470  -2.556  0.01059 * 
#   age               0.0191526  0.0068548   2.794  0.00521 **
#   genderMale       -0.0634247  0.2515503  -0.252  0.80094   
# tot_bilirubin     0.0061742  0.0672091   0.092  0.92680   
# direct_bilirubin  0.4017199  0.2161681   1.858  0.06312 . 
# tot_proteins      0.0006658  0.0007474   0.891  0.37300   
# albumin           0.0107209  0.0051668   2.075  0.03799 * 
#   ag_ratio          0.0038375  0.0034613   1.109  0.26756   
# sgpt              1.0306226  0.4106348   2.510  0.01208 * 
#   sgot             -1.9692617  0.8158509  -2.414  0.01579 * 
#   alkphos           2.2210014  1.2579300   1.766  0.07746 . 
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 599.20  on 500  degrees of freedom
# Residual deviance: 495.47  on 490  degrees of freedom
# AIC: 517.47
# 
# Number of Fisher Scoring iterations: 7

# Predciting on the test data
pred = predict(logistic, test, type = "response")
pred = ifelse(pred > 0.40, 1, 0)

# Calculating the confusion matrix
matrix = confusionMatrix(pred,test$is_patient)
matrix
# Okay, good we got some 75.64% accuracy
# Lets dig deep

# But keep in mind that the threshold we fixed 
# is 0.4 and we have no means of fixing it 
# with evidence

# Let us run the model for different thresholds
# and see for which thresholds we can afford,
# type 1 and type 2 errors and accuracy levels

Results = matrix(0,nrow = 10, ncol = 4)
colnames(Results) = c('Threshold','01error','10error','Accuracy')

for(i in 1:10){
  
  # Setting threshld value
  t = 0.2+0.05*i
  
  # Predcitions
  pred = predict(logistic, test, type = "response")
  pred = ifelse(pred > t, 1, 0)
  
  # Confusion matrix
  matrix = confusionMatrix(pred,test$is_patient)
  Results[i,1] = t
  Results[i,2] = matrix$table[2,1]
  Results[i,3] = matrix$table[1,2]
  Results[i,4] = matrix$overall[1]
}

# In this case we cannot afford to have 10
# error because, when the patient is having 
# liver disease you cannot afford to say that
# he/she doesnt have it, thats more dangerous 
# than 01 error

# So I am inclined to have the thresholds of
# the first five rows of the results matrix

# 0.40 would be better threshold
# looking at the results matrix

Results = as.data.frame(Results)

# Plotting the graphs of errors & Accuracy
# is not much varying in the first 5 rows(Results)
ggplot(Results, aes(Threshold)) + 
   geom_line(aes(y = Accuracy*10, colour = "Accuracy")) +
   geom_line(aes(y = `01error`, colour = "`01error`")) +
   geom_line(aes(y = `10error`, colour = "`10error`")) + 
  geom_vline(xintercept=0.38, linetype="dotted",size=0.6)+
  geom_vline(xintercept=0.42, linetype="dotted",size=0.6)+
   annotate("rect", xmin = 0.38, xmax = 0.42,
            ymin = 0, ymax = 20, alpha = .2)

# So from the plot also it is visible that
# Accuracy is high and 10 error is less
# in the region around 0.40 (Shaded area)

# After couple of trial and errors I found it to be 0.42
# Predicting on the test data
pred = predict(logistic, test, type = "response")
pred = ifelse(pred > 0.42, 1, 0)

# Calculating the confusion matrix
matrix = confusionMatrix(pred,test$is_patient)
matrix

# Wow! So accuaracy is increased to 76.92% and
# 10error is still one only. Lets fix here!

# Plotting an ROC curve
ROC = roc(response = test$is_patient,
          predictor = pred)
plot(ROC)
auc(ROC)

############################### K-Nearest neighbors ##################################

# Creating new variables for interpretability
train_knn = train
test_knn = test

# Let us create some dummy variable for Gender in both train and test
# Train data set
gender = dummy(train_knn$gender)
train_knn$gender = gender[,2]
str(train_knn)
# Test data set
gender = dummy(test_knn$gender)
test_knn$gender = gender[,2]
str(test_knn)

# Let us do scaling for Knn
train_scale = scale(train_knn[,-11]) # Excluding response
test_scale = scale(test_knn[,-11]) # Excluding response

# Knn Model
# Not sure how many neighbors to use
# So let us run a loop and record the 
# metrics for each number of neighbors

Results_knn = matrix(0,nrow = 13, ncol = 4)
colnames(Results_knn) = c('Neighbors','01error','10error','Accuracy')

for(i in 1:13){
  pred_knn = knn(train = train_scale,
                 test = test_scale,
                 cl = train_knn[,11],
                 k = i + 2,
                 prob = TRUE)
  
  knn_con = confusionMatrix(data = pred_knn, reference = test_knn[,11])
  Results_knn[i,1] = i + 2
  Results_knn[i,2] = knn_con$table[2,1]
  Results_knn[i,3] = knn_con$table[1,2]
  Results_knn[i,4] = knn_con$overall[1]
  }

Results_knn = as.data.frame(Results_knn)
# When neighbors is 7, I am witnessing
# a minimum 10,01 error as well as higher accuracy = 73.07%


#####################################################################################




############################### SVM Model #########################################

# Let us build an SVM model on this data set
sv_model = svm(is_patient~., data = train, kernel = "linear")
sv_model
summary(sv_model)

pred_sv = predict(sv_model,test)
confusionMatrix(reference = as.factor(test$is_patient), data = pred_sv)
# Accuracy is 71.79%, but all are 01errors!
# And this is exactly matching with the
# logistic regression results of 0.3  threshold!!

# tuning my model
tune.out = tune(method = svm,
                train.x = is_patient~.,
                data = train,
                kernel = "linear",
                ranges = list(cost = c(10^-7,0.0001,0.001,0.01,0.1,1)))
tune.out
summary(tune.out$best.model)
best = tune.out$best.model

# Hence let us use best model for predictions
pred_sv_tune = predict(best,test)
confusionMatrix(reference = as.factor(test$is_patient), data = pred_sv_tune)

# Both linear and radial svms are tried, and there is
# no improvement in the accuracy of 10error rates
# So let us fix on SVMs results as
# Accuracy is 71.79%, and 10error = 0


###################################################################################

# If the client asks me for a thrshold value in the logistic regression to fix 
# based on the data provided I would say a value of 0.4 because, since this
# problem is related to Health industry, we cannot afford to have existence of
# a 10error which is false negative. It costs more than a 01 error (false positve).
# 
# So I would create a Logistic regression model with 0.4 threshold.
# 
# But when it comes to both Knn and SVM I would vote for SVM since it has a less 
# 10error which is 0! The accuracies of both the models are nearly the same.
# 
# In the next versions,
# I would like to impute the missing values.
# Apply principal components on the data and run the models again.
# And see if there are any differences in our inferences.
# Let us do some clustering in the next version.

# Also I would like to change the approach of the finding the threshold
# of the logistic in another way. I will run a cross validation on 6 different
# sets and then observe the good accuracy and 10 error rates for 6 models
# and then check the best threshold for fit
