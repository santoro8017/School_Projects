#initialize environment
library(datasets)
library(ggplot2)

cat("\014")
rm(list=ls())

#explore clustering using the built in dataset iris
data<-iris
head(iris)

#how many plants are in each species or iris
table(iris[,5], iris$Species)

#plot data for overview of dataset
ggplot(data, aes(Petal.Length, Petal.Width, color = Species)) + geom_point()

#set seed to remove randomness 
set.seed(888)

#explore the optimal number of clusters (k) by using within cluster sum of squares
k.max<-10
wss<-sapply(1:k.max,function(k){kmeans(iris[,3:4],k,nstart=20)$tot.withinss})
plot(1:k.max,wss, type= "b", xlab = "Number of clusters(k)", ylab = "Within cluster sum of squares")

#perform kmeans using the optimal number of clusters found above (3)
irisCluster <- kmeans(iris[,4],3,nstart=20)

#print a table to represent accuracy
print(table(irisCluster$cluster, iris$Species))
