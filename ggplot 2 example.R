library(ggplot2)

auto = read.table("E:/YVC/.../Datasets/auto.csv", sep = ",",header = T)

str(auto)

auto$origin = factor(auto$origin)
levels(auto$origin) = c("American", "European", "Japanese")

auto$cylinders = factor(auto$cylinders)

plot(mpg ~ horsepower, data = auto)

# Scatter Plot
ggplot(data = auto) + geom_point(mapping = aes(y = mpg, x = horsepower),col="blue", size = 1, shape = 2)

ggplot(data = auto) + geom_point(mapping = aes(y = mpg, x = horsepower, col = origin), size = 1)

ggplot(data = auto) + geom_point(mapping = aes(y = mpg, x = horsepower, col = origin), size = 1)+
  geom_smooth(mapping = aes(y = mpg, x = horsepower), method = "lm")

ggplot(data = auto) + geom_point(mapping = aes(y = mpg, x = horsepower), size = 1)+
  geom_smooth(mapping = aes(y = mpg, x = horsepower), method = "loess")+
  facet_wrap(~origin, nrow = 2)


ggplot(data = auto) + geom_point(mapping = aes(y = mpg, x = horsepower), size = 1)+
  geom_smooth(mapping = aes(y = mpg, x = horsepower), method = "loess")+
  facet_grid(cylinders~origin)


# Bar Chart
ggplot(data = auto) + geom_bar(mapping = aes(x = origin))

ggplot(data = auto) + geom_bar(mapping = aes(x = origin, fill = cylinders))

ggplot(data = auto) + geom_bar(mapping = aes(x = origin, fill = cylinders), position = "dodge")


# Box Plot
ggplot(data = auto) + geom_boxplot(mapping = aes(x = origin, y = mpg, fill = origin))

ggplot(data = auto) + geom_boxplot(mapping = aes(x = origin, y = mpg, fill = origin)) + coord_flip()


# Histogram
ggplot(data = auto) + geom_histogram(mapping = aes(x = mpg), fill = "yellow",col = "blue", bins = 10)+
  geom_freqpoly(mapping = aes(x = mpg),col = "red", bins = 10)

# Density Plot
ggplot(data = auto) + geom_density(mapping = aes(x = mpg, fill = origin), alpha = 0.3)



