table(chdage$chd,chdage$agegrp)

#choosing training set and testing set
smp_size = floor(0.7 * nrow(chdage))
set.seed(123)
train_index = sample(1:nrow(chdage), smp_size)
train = chdage[train_index, ]
test = chdage[-train_index, ]

# Logistic regression on train set
train$chd = factor(train$chd)
logistic1 = glm(chd ~ age , data = train, family = binomial)
summary(logistic1)
round(exp(0.12111*10),2) # odds ratio for an increase of 10 years

# prediction
probs = predict(logistic1, newdata = test, type = "response")
myPrediction = rep("No",nrow(test))
myPrediction[probs > 0.5] = "Yes"

# Evaluation
table(predicted = myPrediction, actual = test$chd)

goodness.of.classification(5,13,5,7)


# Residual deviance = -2Log Likelihood
emptymodel = glm(chdage$chd ~ 1, family = binomial)
library(lmtest)
lrtest(emptymodel, model1)