library(MASS)


#iris dataset
iris

#number of rows in iris dataset
length(rownames(iris))

#number of columns in iris dataset
length(colnames(iris))

#summary of sepal.length
summary(iris$Sepal.Length)

#summary of sepal.width
summary(iris$Sepal.Width)

#type of species and its number
summary(iris$Species)

#making a datset with size of petal length is greater than 2

dataset2<- subset(iris, Petal.Length>2)
dataset2
