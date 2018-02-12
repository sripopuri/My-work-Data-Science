# Loading the required libraries
library(rpart)
library(partykit) # Treeplots
library(MASS)
library(ElemStatLearn)
library(randomForest)
library(gbm)
library(caret)

# Checking the summary of data
data("prostate")
str(prostate)
summary(prostate)

prostate$gleason = ifelse(prostate$gleason == 6, 0,1)
train = subset(prostate, train = TRUE)[,1:9]
test = subset(prostate, train = FALSE)[,1:9]

# Creating the Decision Tree
tree = rpart(lpsa~., data = train)
print(tree$cptable)

# Plotting the Tree
plotcp(tree)

plot(tree, uniform = TRUE, margin = 0.1, branch = 0.5, compress = TRUE)
text(tree)

# Pruning the Tree
mincp = min(tree$cptable[6,])
prune = prune(tree, cp = mincp)

# Plot the Pruned Tree
plot(prune, uniform = TRUE, margin = 0.1, branch = 0.5, compress = TRUE)
text(prune)

plot(as.party(tree))
plot(as.party(prune))


# Classification
data("biopsy")
biopsy = biopsy[,-1]
names(biopsy) = c("thick","u.size","u.shape","adhsn","s.size","nucl",
                  "chrom","n.nuc","mit","class")
biopsy.v2 = na.omit(biopsy)
set.seed(123)
ind = sample(2,nrow(biopsy.v2),replace = T, prob = c(0.7,0.3))
train = biopsy.v2[ind == 1,]
test = biopsy.v2[ind == 2,]

str(test[,10])

set.seed(123)
tree = rpart(class~., data = train)
tree$cptable

mincp = min(tree$cptable[3,])
prune = prune(tree, cp = mincp)

plot(tree, uniform = TRUE, margin = 0.1, branch = 0.5, compress = TRUE)
text(tree)
plot(prune, uniform = TRUE, margin = 0.1, branch = 0.5, compress = TRUE)
text(prune)

plot(as.party(tree))
plot(as.party(prune))


predict(tree, newdata = test, type = "class")
table(predict(tree, newdata = test, type = "class"), test$class)
sum(diag(table(predict(tree, newdata = test, type = "class"), test$class)))
