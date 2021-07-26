# OJ - sales data for citrus hill and minute maid orange juice (purchase)
library(ISLR)
attach(OJ)
set.seed(1013)

train = sample(dim(OJ)[1], 800)
OJ.train = OJ[train, ]
OJ.test = OJ[-train, ]

library(tree)
oj.tree = tree(Purchase ~ ., data = OJ.train)
summary(oj.tree)

oj.tree

plot(oj.tree)
text(oj.tree, pretty = 0)

oj.pred = predict(oj.tree, OJ.test, type = "class")
table(OJ.test$Purchase, oj.pred)

cv.oj = cv.tree(oj.tree, FUN = prune.tree)

plot(cv.oj$size, cv.oj$dev, type = "b", xlab = "Tree Size", ylab = "Deviance")

oj.pruned = prune.tree(oj.tree, best = 6)

summary(oj.pruned)
# Misclassification error of pruned tree is exactly same as that of
# original tree 0.155.

pred.unpruned = predict(oj.tree, OJ.test, type = "class")
misclass.unpruned = sum(OJ.test$Purchase != pred.unpruned)
misclass.unpruned/length(pred.unpruned)

pred.pruned = predict(oj.pruned, OJ.test, type = "class")
misclass.pruned = sum(OJ.test$Purchase != pred.pruned)
misclass.pruned/length(pred.pruned)
# Pruned and unpruned trees have same test error rate of 0.189.