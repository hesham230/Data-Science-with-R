library(tree)
library(ISLR)
data(Carseats)
#?Carseats
str(Carseats)

# Turning the dependent var into categorical
Carseats$Sales = as.factor(ifelse(Carseats$Sales <= 8, "Low", "High"))
str(Carseats)

# Running a classification tree
seat_tree = tree(Sales ~ ., data = Carseats)

# Plotting Tree
summary(seat_tree)
plot(seat_tree)
text(seat_tree, pretty = 0)
title(main = "Unpruned Classification Tree")

seat_tree

dim(Carseats)

# Dividing the data into training and testing
set.seed(2)
seat_idx = sample(1:nrow(Carseats), 200)
seat_trn = Carseats[seat_idx,]
seat_tst = Carseats[-seat_idx,]

# Running classification tree on the training set only
seat_tree = tree(Sales ~ ., data = seat_trn)

summary(seat_tree)

# Used and unused variables
summary(seat_tree)$used
names(Carseats)[which(!(names(Carseats) %in% summary(seat_tree)$used))]

plot(seat_tree)
text(seat_tree, pretty = 0, cex = 0.4)
title(main = "Unpruned Classification Tree")

seat_trn_pred = predict(seat_tree, seat_trn, type = "class") # train prediction
seat_tst_pred = predict(seat_tree, seat_tst, type = "class") # test prediction

# train confusion
table(predicted = seat_trn_pred, actual = seat_trn$Sales)

# test confusion
table(predicted = seat_tst_pred, actual = seat_tst$Sales)

accuracy = function(actual, predicted)
{
  mean(actual == predicted)
}

# train accuracy
accuracy(predicted = seat_trn_pred, actual = seat_trn$Sales)

# test accuracy
accuracy(predicted = seat_tst_pred, actual = seat_tst$Sales)

# Cross validation on tree
set.seed(3)
seat_tree_cv = cv.tree(seat_tree, FUN = prune.misclass)

plot(seat_tree_cv$size , seat_tree_cv$dev, type = 'b')

# index of tree with minimum error
min_idx = which.min(seat_tree_cv$dev)
min_idx

# number of terminal nodes in that tree
seat_tree_cv$size[min_idx]

# misclassification rate of each tree
seat_tree_cv$dev / length(seat_idx)

par(mfrow = c(1, 2))
# default plot
plot(seat_tree_cv)
# better plot
plot(seat_tree_cv$size, seat_tree_cv$dev / nrow(seat_trn), type = "b",
     xlab = "Tree Size", ylab = "CV Misclassification Rate")

# Pruning
seat_tree_prune = prune.misclass(seat_tree, best = 9)
summary(seat_tree_prune)

plot(seat_tree_prune)
text(seat_tree_prune, pretty = 0, cex =0.5)
title(main = "Pruned Classification Tree")

# Train predictions prunned tree
seat_prune_trn_pred = predict(seat_tree_prune, seat_trn, type = "class")
table(predicted = seat_prune_trn_pred, actual = seat_trn$Sales)

accuracy(predicted = seat_prune_trn_pred, actual = seat_trn$Sales)

# Test predictions prunned tree
yhat = predict(seat_tree_prune, newdata = seat_tst, type = "class")
table(predicted = yhat, actual = seat_tst$Sales)

accuracy(predicted = seat_prune_tst_pred, actual = seat_tst$Sales)

# sensitivity
58/(58+25)
