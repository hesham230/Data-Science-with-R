library(ISLR)
library(ggplot2)
library(tree)
library(randomForest)
data(Hitters)

Hitters = na.omit(Hitters)

train.index = sample(1:nrow(Hitters), nrow(Hitters)/2)

# Bagging
set.seed(1)
bag.hitters = randomForest(Salary~., data = Hitters, subset = train.index, mtry = 19, importance = T)
bag.hitters


# Random Forest
set.seed(1)
rf.Hitters = randomForest(Salary~., data = Hitters, subset = train.index, mtry = 6, importance = T)
# default for mtry is to use p/3 variables or sqrt(p) for classification
yhat.rf = predict(rf.Hitters, newdata = Hitters[-train.index,])
mean((yhat.rf - Hitters$Salary[-train.index])^2)
importance(rf.Hitters)
varImpPlot(rf.Hitters)
# high values are better