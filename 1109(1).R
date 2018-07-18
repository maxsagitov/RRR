library(ggplot2)
ggplot(airquality, aes(x = factor(Month), y = Ozone))+
  geom_boxplot()
boxplot(airquality, x = Month, y = Ozone)
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, col = Species)) +
  geom_point(aes(size = Petal.Length))