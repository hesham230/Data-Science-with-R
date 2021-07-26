library(readxl)
library(ggplot2)
diabetes = read_excel("D:/Technion - Data Science/Datasets/Diabetes/diabetes.xlsx")

data = diabetes

#factor of y
data$Outcome = factor(data$Outcome) 
levels(data$Outcome) = c("No Diabetes","Diabetes")

# normalizing numeric variables
num.vars = sapply(data, is.numeric)
data[num.vars] = lapply(data[num.vars], scale)
summary(data)

#choosing training set and testing set
smp_size = floor(0.7 * nrow(data))
set.seed(123)
train_index = sample(seq_len(nrow(data)), size = smp_size) # sample from seq of row numbers
train.x = data[train_index, ]
test.x = data[-train_index, ]


# removing the dependent variable from train and test sets
depvar = names(train.x) %in% c("Outcome")
train.x = train.x[!depvar]
test.x = test.x[!depvar]

# creating vectors of dependent variable for both sets
train.y = data$Outcome[train_index]
test.y = data$Outcome[-train_index]


# knn
library(class)
knn.1 =  knn(train.x, test.x, train.y, k=1)
knn.3 =  knn(train.x, test.x, train.y, k=3)
knn.5 =  knn(train.x, test.x, train.y, k=5)
knn.7 =  knn(train.x, test.x, train.y, k=7)
knn.9 =  knn(train.x, test.x, train.y, k=9)
knn.11 =  knn(train.x, test.x, train.y, k=11)
knn.21 =  knn(train.x, test.x, train.y, k=21)


# proportion of correct classification for k = 1, 3, 5
100 * sum(test.y == knn.1)/length(test.y)  # For knn = 1
100 * sum(test.y == knn.3)/length(test.y)  # For knn = 3
100 * sum(test.y == knn.5)/length(test.y)  # For knn = 5
100 * sum(test.y == knn.7)/length(test.y)  # For knn = 7
100 * sum(test.y == knn.9)/length(test.y)  # For knn = 9
100 * sum(test.y == knn.11)/length(test.y)  # For knn = 11
100 * sum(test.y == knn.21)/length(test.y)  # For knn = 21

accuracy = rep(0,20)
k = rep(0,20)
j = 1
# Looping over various values of k
for(i in seq(from=1, to=39, by=2))
{
  knn.i =  knn(train.x, test.x, train.y, k=i)
  accuracy[j] = 100 * sum(test.y == knn.i)/length(test.y)
  k[j]=i
  j = j +1
}

accuracyD = data.frame(k,accuracy)