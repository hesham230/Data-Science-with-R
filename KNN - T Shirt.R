# In this example we have 18 data points of heights and weights of people
# and their shirt size. We get a new observation with height 161 and weight 61
# We create a KNN classification model "by hand"

library(readxl)
library(ggplot2)
Shirts = read_excel("E:/YVC/Data Analysis Lab (Information Systems)/Section B - Statistical Learning/02 - KNN/T Shirt.xlsx")

str(Shirts)

# Standartization to maintain the same scale for both predictors
Shirts$ZHeight = scale(Shirts$Height)
Shirts$ZWeight = scale(Shirts$Weight)

# Standartization of the testing row
# We will use the same mean and SD: In real life problems this is tricky
# we use Z-Scores and assume that the mean and SD in the training and testing
# are equal.
ZH = (161-mean(Shirts$Height))/sd(Shirts$Height)
ZW = (61-mean(Shirts$Weight))/sd(Shirts$Weight)

# Distance of the testing point from each training point
Shirts$Distance = sqrt((Shirts$ZHeight-ZH)^2+(Shirts$ZWeight-ZW)^2)


# Sorting by distance
Shirts = Shirts[order(Shirts$Distance),]


# KNN Classification (default example: 5NN)
k = 5
M = 0
L = 0
for (i in 1:k)
{
    if (Shirts$ShirtSize[i] == 'M')
    {
      M = M + 1
    }
   else
   {
      L = L +1
    }
}

# ggplot(Shirts, aes(x=ZHeight, y=ZWeight, shape=ShirtSize, color=ShirtSize, size = 2)) + geom_point()

new = c(161,61,"X",ZH,ZW,0)
Shirts2 = rbind(Shirts,new)

Shirts2$ZHeight = as.numeric(Shirts2$ZHeight)
Shirts2$ZWeight = as.numeric(Shirts2$ZWeight)
Shirts2$Distance = as.numeric(Shirts2$Distance)

Shirts2$ZHeight = round(Shirts2$ZHeight,2)
Shirts2$ZWeight = round(Shirts2$ZWeight,2)
Shirts2$Distance = round(Shirts2$Distance,2)

ggplot(Shirts2, aes(x=ZHeight, y=ZWeight, shape=ShirtSize, color=ShirtSize, size = 2)) +
  geom_point() + geom_text(aes(label=Distance),hjust=0.5, vjust=1.5)
