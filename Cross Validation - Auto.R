library(ISLR)
mpg = Auto
str(mpg)

##############################################################################
# Validation Set Approach                                                   ##
##############################################################################
# when a testing set is not available, we can create validation sets         #
# if we fit two models for two different validation sets, we get somewhat    #
# similar results.                                                          ##
##############################################################################
# First Model
set.seed(1)
train_index = sample(length(mpg$mpg),0.5*length(mpg$mpg))

lm1 = lm(mpg ~ horsepower, data = mpg, subset = train_index)
trainSet = mpg[train_index,]
validSet = mpg[-train_index,]

summary(lm1)

EMSE1 = mean((validSet$mpg - predict(lm1, newdata = validSet))^2)

# Model 2
set.seed(2)
train_index = sample(length(mpg$mpg),0.5*length(mpg$mpg))

lm2 = lm(mpg ~ horsepower, data = mpg, subset = train_index)
trainSet = mpg[train_index,]
validSet = mpg[-train_index,]

summary(lm2)

EMSE2 = mean((validSet$mpg - predict(lm2, newdata = validSet))^2)


##############################################################################
## The data is divided randomly into K groups. For each group the generalized#
## linear model is fit to data omitting that group, then the function cost is#
## applied to the observed responses in the groupthat was omitted from the   #
## fit and the prediction made by the fitted models for those observations.  #
## When K is the number of observations leave-one-out cross-validation is    #
## used and all the possiblesplits of the data are used.                     #
## When K is less than the number of observations the K splits to be used    #
##############################################################################


##############################################################################
## Leave One Out Cross Validation ############################################
##############################################################################
fit.glm = glm(mpg ~ horsepower, data = mpg) #without family = , we get linear regression
coef(fit.glm)

library(boot)
cv.err = cv.glm(data = Auto, glmfit = fit.glm, K = nrow(mpg))
cv.err$delta
cv.err$K # when k = n R will do a leave one out cross validation

##############################################################################
##### K Fold CV ##############################################################
##############################################################################
cv.error.10 = cv.glm(data = mpg, glmfit = fit.glm, K = 10)$delta[2]
cv.error.5 = cv.glm(data = mpg, glmfit = fit.glm, K = 5)$delta[2]


# Cost function for binary outcome
# cost <- function(r, pi = 0) mean(abs(r-pi) > 0.5)