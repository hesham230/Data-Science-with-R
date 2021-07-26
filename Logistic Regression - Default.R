########### Topic: Logistic Regression ##############
########### Dataset: Default ########################
options(scipen = 999)
# Reading Data
library(ISLR)
D = Default

# Verifying that the dependent variable is defined as a factor
D$default = factor(D$default)
contrasts(D$default)

# Descriptive Statistics
library(ggplot2)
table(D$default)
ggplot(data = D) + geom_point(mapping = aes(x = balance, y = income, colour = default:student))

# Simple Logistic Regression
glm1 = glm(default ~ balance, data = D, family = binomial)
summary(glm1)

glm2 = glm(default ~ student, family = binomial, data = D)
summary(glm2)
contrasts(D$student)

# Extracting coefficients
coef(glm1)
coef(glm1)[1]
coef(glm1)[2]

# Calculating predictiobs on training set
glm.predictions = predict(glm11, type = "response") #predictions for training data on P(Y=1|X)
-10.651330614+0.005498917*729.5265
exp(-6.639725)/(1+exp(-6.639725))
glm.predictions[1]

predictions = rep("No",length(D$default))
predictions[glm.predictions>0.5] = "Yes"
table(predictions,D$default)  # Training confusion matrix


#create artificial test
D.test = D[1:200,]
D.train = D[201:10000,]

# model training set
model3 = glm(default~balance,data = D.train, family = binomial)


# predictions for test set
test.predictions = predict(model3, D.test , type = "response")
predictions = rep("No",length(D.test$default))
predictions[test.predictions>0.5] = "Yes"
confusion1 = table(predictions,D.test$default)  # Testing confusion matrix

# OR for more than 1 unit increase
# exp(units*beta)

# Multiple Logistic Regression
set.seed(7)
train.size = floor(0.7*nrow(D))
train.index = sample(1:nrow(D),train.size)
train.set = D[train.index,]
test.set = D[-train.index,]

glm3 = glm(default ~ student + balance, family = binomial, data = train.set)
summary(glm3)

predictions = rep("No",nrow(test.set))
test.predictions = predict(glm3, newdata = test.set , type = "response")
predictions[test.predictions>0.5] = "Yes"
confusion1 = table(predictions,test.set$default)  # Testing confusion matrix

# Goodness of Predictions
TP = confusion1[4]
TN = confusion1[1]
FP = confusion1[2]
FN = confusion1[3]

accuracy = (TP+TN)/(TP+TN+FP+FN)
sensitivity = TP/(TP+FN)
specificity = TN/(TN+FP)
PPV = TP/(TP+FP)
NPV = TN/(TN+FN)
F1Score = (2*TP)/(2*TP+FP+FN)

# ROC Curve Analysis
library(pROC)
roc(response = test.set$default, predictor = test.predictions, auc = T, plot = T)
# Finding optimal point on roc curve using Youden J Index
roc1 = roc(response = test.set$default, predictor = test.predictions, auc = T, plot = T)
coords(roc1, x="best", input="threshold", best.method="youden", transpose = T)


# Likelihood Ration Test

# log likelihood model = -570.75 Residual deviance = -2log Likelihood =  1141.5 
# log likelihood empty model = -1052.05  null deviance - 2104.1
# Likelihood Ratio Test = Deviance reduced - Deviance full or -2*((LL Reduced) - (LL Full))
# Likelihood Ratio Test = -2*(-1052.03 + 572.27)
# Distribution is chi square with p-r degrees of freedom (number of vars added to the model)
emptymodel = glm(data = train.set, default ~ 1, family = binomial)
library(lmtest)
lrtest(emptymodel, glm3)
LR.Statistic = -2*((-691.63) - (-690.55))
LR.Statistic = logistic1$null.deviance - logistic1$deviance
LR.PValue = 1-pchisq(959.52,4)