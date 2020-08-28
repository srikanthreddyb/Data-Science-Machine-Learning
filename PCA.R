library(datasets)
data(iris)
summary(iris)
iris
irispca<-princomp(iris[-5])
summary(irispca)
screeplot(irispca)
irispca$loadings

