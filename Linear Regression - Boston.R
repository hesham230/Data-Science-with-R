# Reading the Data
library(MASS)
Data = Boston

# Looking at the Data
str(Data)

#Checking the distribution of medv
hist(Data$medv)
qqnorm(Data$medv)
qqline(Data$medv)
shapiro.test(Data$medv)

cor(Data[c("medv", "lstat", "age", "rm","ptratio")])
pairs(Data[c("medv", "lstat", "age", "rm","ptratio")])
library(psych)
pairs.panels(Data[c("medv", "lstat", "age", "rm","ptratio")])

#choosing training set and testing set
smp_size = floor(0.7 * nrow(Data))
set.seed(123)
train_index = sample(1:nrow(Data), smp_size)
train.set = Data[train_index, ]
test.set = Data[-train_index, ]

# Fitting Regression Model on training data
m = lm(medv ~ lstat + age + rm + ptratio, data = train.set)
coefficients(m)
summary(m)

# Prediction on the testing set
p = predict(m , newdata = test.set)
plot(test.set$medv,p)


test.mse = mean((test.set$medv - p)^2)




########################### KNN ############################################

library(caret)

trainvars = c("lstat", "age", "rm","ptratio")
trainX = train.x[trainvars]

traindepvar = c("medv")
trainY = train.x[traindepvar]

testvars = c("lstat", "age", "rm","ptratio")
testX = test.x[testvars]

testdepvar = c("medv")
testY = test.x[testdepvar]

knn3 = knnreg(trainX, trainY$medv, k = 3)
plot(testY$medv, predict(knn3, testX))
e_knn3 = testY$medv - predict(knn3, testX)

mean(e_knn3^2)