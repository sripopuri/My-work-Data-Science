# clearing my workspace
rm(list = ls())

# Loading the required Libraries
library(ISLR)
library(e1071)
library(caret)

# Getting the dataset
dat = ISLR::OJ
index = sample(nrow(dat),800)
train = dat[index,]
test = dat[-index,]

# Classifier
classif = svm(Purchase~., data = train,
              kernel = "linear", cost = 0.01)
summary(classif)

# There are 443 data points as
# support vectors out of 800
# 221 belong to CH and 222 belong to MM classes

pred1 = predict(classif, train)
confusionMatrix(pred1, train$Purchase)

# Error rate is 1-0.8275 = 0.1725

pred2 = predict(classif, test)
confusionMatrix(pred2, test$Purchase)

# Error rate is 1-0.8407 = 0.1593

################# LINEAR KERNEL
tuning = tune(svm, Purchase~., data = train, kernel = "linear",
              ranges = list(cost = seq(from = 0.01, to = 10,by = 0.01)))
summary(tuning$best.model)

bestmodel = tuning$best.model

pred3 = predict(bestmodel, train)
confusionMatrix(pred3, train$Purchase)

# Error Rate is 1 - 0.8312 = 0.1688

pred4 = predict(bestmodel, test)
confusionMatrix(pred4, test$Purchase)

# Error Rate is 1 - 0.8481 = 0.1519

# There is a reduction in both, 
# training and test error rates
# which is evident above

################# RADIAL KERNEL

classrad = svm(Purchase~., data = train,
              kernel = "radial")
summary(classrad)
# Support vectors are 383 out if 800 points
# Default value for Gamma is 0.05556
# 185 of the support vectors belong to CH
# and 198 belong to MM classes respectively

pred5 = predict(classrad, train)
confusionMatrix(pred5, train$Purchase)

# Error rate is 1- 0.845 = 0.155

pred6 = predict(classrad, test)
confusionMatrix(pred6, test$Purchase)

# Error rate is 1- 0.8444 = 0.1556

tuningrad = tune(svm, Purchase~., data = train, kernel = "radial",
                 ranges = list(gamma = seq(from = 0.01, to = 10,by = 0.05)))
summary(tuningrad$best.model)

# So the best model is having a cost value of 1
# and gamma value of 0.06

bestmodelrad = tuningrad$best.model

pred7 = predict(bestmodelrad, train)
confusionMatrix(pred7, train$Purchase)

# Error Rate is 1 - 0.8462 = 0.1538

pred8 = predict(bestmodelrad, test)
confusionMatrix(pred8, test$Purchase)

# Error Rate is 1 - 0.8444 = 0.1556

# There is a slight improvement in the Training error rate
# but no improvement in the test error rate
# These parameters are already in the model with the
# default values, see model 'classrad'
# Hence not much improvement

################# POLYNOMIAL KERNEL

classpoly = svm(Purchase~., data = train,
               kernel = "polynomial")
summary(classpoly)
# Support vectors are 422 out if 800 points
# Default value for Gamma=0.05556, cost = 1, degree = 3
# 208 of the support vectors belong to CH
# and 214 belong to MM classes respectively

tuningpoly = tune(svm, Purchase~., data = train, kernel = "polynomial",degree = 2,
                  ranges = list(cost = seq(from = 0.01, to = 10,by = 0.05)))
summary(tuningpoly$best.model)
# So the best model is having a cost value of 6.66
# and gamma value of 0.05556, degree = 2

bestmodelpoly = tuningpoly$best.model

pred9 = predict(bestmodelpoly, train)
confusionMatrix(pred9, train$Purchase)

# Error Rate is 1 - 0.8462 = 0.1538

pred10 = predict(bestmodelpoly, test)
confusionMatrix(pred10, test$Purchase)

# Error Rate is 1 - 0.8259 = 0.1741

# To chose the best model, I would chose the one with the least 
# Test error rate.
# The test error rates for all the three kernel's best models are
# Linear - 0.1519
# Radial - 0.1556
# Polynomial - 0.1741
# So Linear kernel in this problem fits good.
# And these differences between linear and radial
# are minute and again business knowledge needs to come inhere
# and solve the final selection I believe