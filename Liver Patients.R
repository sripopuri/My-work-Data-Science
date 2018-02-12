# Clearing the workspace 
rm(list = ls())  

# Loading the required packages
library(dummies)

# Setting the required working directory
#setwd("C:/Users/Sri Harish/Desktop/Kaggle/Liver patient Dataset")

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

# The is_patient field should be as a factor
# So let us encode it using dummy values along with gender variable
is_patient = dummy(data$is_patient)
data$is_patient = is_patient[,1]
gender = dummy(data$gender)
data$gender = gender[,2]
str(data)

# Now let us factorize this variable and gender variable
data$is_patient = as.factor(data$is_patient)
data$gender = as.factor(data$gender)
str(data)
# 0 for Female
# 1 for Being a live patient

# let us create a feature called Globulin
# We have ag_ration and Albumin from which
# we can create this new feature
#data$globulin = round(data$albumin/data$ag_ratio,2)

nona = data[complete.cases(data),]
corrplot::corrplot(cor(nona[,-c(2,11)]), 
                   type = "lower",
                   diag = FALSE,
                   addCoef.col = "black")

# From Corrplot we can say that 
# both Total bilirubin dn direct bilirubin
# are highly correlated, 
# Obviously Direct bilirubin should 
# have been part of total Bilirubin 
#same is the case with spot and sgot
# Similarly Albumin and agratio
# and agot and alkphos are correlated
# to great extent

plot(nona[,-c(2)])

colnames(data)

# SVM
svm = svm(is_patient~., data = nona[-574:-583,], kernel = "radial")
svm
summary(svm)
y = predict(svm,nona[574:583,-11])
confusionMatrix(nona[574:583,11],y)

colnames(data)
lin1 = lm(direct_bilirubin~tot_bilirubin, data = data)
summary(lin1)
plot(lin1)

lin2 = lm(sgot~sgpt, data = data)
summary(lin2)
plot(lin2)

