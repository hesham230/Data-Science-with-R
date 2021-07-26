####################
# Calling Packages #
####################
library(ISLR)
library(tree)
library(MASS)
library(ggplot2)
B = Boston

#####################################
# Randomization of training samples #
#####################################
set.seed(1)
train.index = sample(1:nrow(B),nrow(B)/2)
train.set = B[train.index,]
test.set = B[-train.index,]

#####################################
#      Fitting a Tree Model         #
#####################################
tree.boston = tree(medv ~ . , data = train.set)
summary(tree.boston)
plot(tree.boston)
text(tree.boston,pretty = 0, digits = 3)

# If pretty = 0 then the level names of a factor split attributes are used unchanged.
# If pretty = NULL, the levels are presented by a, b, z, 0 5.
# If pretty is a positive integer, abbreviate is applied to the labels with that
# value for its argument minlength.


###########################################################
#      Cross Validation for choosing the best tree        #
###########################################################
cv.boston = cv.tree(tree.boston) # default is K = 10 folds
plot(cv.boston$size,cv.boston$dev, type = 'b') # b is line style (with/without dots)
ggplot(mapping = aes(x=cv.boston$size, y=cv.boston$dev)) + geom_line() + geom_point()

# According to the cv.tree plot, we see that the most complex tree is selected
# by the cross validation. However, we will prune the tree to 6 nodes.

#####################################
#      Pruning the tree             #
#####################################
prune.boston = prune.tree(tree.boston, best = 6)
plot(prune.boston)
text(prune.boston,pretty = 0)


###################################################
#      Making Predictions on the test set         #
###################################################
# Using the best tree according to cv
yhat = predict(prune.boston, newdata = test.set)
plot(test.set$medv, yhat)
abline(0,1)
mean((test.set$medv - yhat)^2) # test MSE
