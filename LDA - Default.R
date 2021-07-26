##################################
##        LDA Example           ##
##################################

# Import Packages
library(ISLR)
library(MASS)

# Import Data
D = Default


# Train and Test
train.size = floor(0.7*nrow(D))
set.seed(7)
train.index = sample(1:nrow(D), train.size)
train.set = D[train.index,]
test.set = D[-train.index,]


# Descriptive Statistics
table(train.set$default)
prop.table(table(train.set$default))

# LDA 1 var
lda.fit = lda(default ~ balance , data = train.set)
lda.fit
plot(lda.fit)

lda.pred = predict(lda.fit, newdata = test.set)
lda.calssification = lda.pred$class
table(lda.classification,test.set$default)

lda.posterior = lda.pred$posterior
lda.posterior[,1] # a matrix who's kth column contains the posterior probabilities that the observatiob belongs to the kth class