library(class)
library(gmodels)
# Import Data
wbc = read.table("E:/Technion - Data Science/Statistical Learning/Lectures/02 - KNN/wisc_bc_data.csv", sep = ",", header = T)

# Examine Data
str(wbc)

# Dropping ID Variable
wbc = wbc[,-1]

# Frequency Table
table(wbc$diagnosis)
# Relative frequency
round(prop.table(table(wbc$diagnosis))*100, digits = 1)

# Target to factor
wbc$diagnosis = factor(wbc$diagnosis, levels = c("B","M"),labels = c("Benign","Malignant"))

# Summary to show why normalization is needed
summary(wbc[c("radius_mean","area_mean")])

# Transformation
normalize = function(x)
{
  return((x-min(x))/(max(x)-min(x)))
}

wbc_n = sapply(wbc[2:31], normalize) # creates a matrix
wbc_n = as.data.frame(wbc_n)
summary(wbc_n[c("radius_mean","area_mean")])

# Training and Testing for KNN
train.x = wbc_n[1:469,]
test.x = wbc_n[470:569,]
train.y = wbc[1:469,1]
test.y = wbc[470:569,1]

# KNN
knn.3 = knn(train = train.x, test = test.x, cl = train.y, k = 3)
knn.5 = knn(train = train.x, test = test.x, cl = train.y, k = 5)


# Evaluation
table(predicted = knn.3 , actual = test.y)
table(predicted = knn.5 , actual = test.y)
CrossTable(x = knn.3, y = test.y, prop.chisq = F)
