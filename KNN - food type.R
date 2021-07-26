############## Food Example #############
library(readxl)
library(class)
food = read_excel("E:/Technion - Data Science/Statistical Learning/Lectures/02 - KNN/food type.xlsx")

food2 = food

food2$Sweetness = scale(food2$Sweetness)
food2$Crunchiness = scale(food2$Crunchiness)

train.x = food2[-16,-4]
train.y = food2[-16,4]

test.x = food2[16,-4]
test.y = food2[16,4]

train.x = train.x[,-1]
test.x = test.x[,-1]


knn.5 =  knn(train = train.x, test = test.x, cl = t(train.y), k=5)

knn.5

knn.3 =  knn(train = train.x, test = test.x, cl = t(train.y), k=3)

knn.3

knn.7 =  knn(train = train.x, test = test.x, cl = t(train.y), k=7)

knn.7