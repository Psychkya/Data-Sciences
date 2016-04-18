library(MASS)
data("iris")
str(iris)
#we remove the species feature
iris.z <- iris[,-5]
str(iris.z)
#normalize iris
iris.znorm <- as.data.frame(lapply(iris.z, scale))
str(iris.znorm)
#find elbow
kval <- 2:14
set.seed(1)
WSS <- sapply(kval, function(k){kmeans(iris.znorm, centers = k)$tot.withinss})
plot(kval,WSS,type = "l", xlab = "number of k", ylab = "within sum of squares")
#Interesting point around k=5. the ss value actually increases for a bit after k=5. Try with max-min norm
normalize <- function(x){ return((x - min(x))/(max(x) - min(x)))}
iris.znorm2 <- as.data.frame(lapply(iris.z, normalize))
str(iris.znorm2)
WSS <- sapply(kval, function(k){kmeans(iris.znorm, centers = k)$tot.withinss})
plot(kval,WSS,type = "l", xlab = "number of k", ylab = "within sum of squares")
#Better elbows, but again weird behavior after k=13
#We will go with k=4
set.seed(1234)
iris.clusters <- kmeans(iris.znorm2, 4)
#size of clusters
iris.clusters$size
#Examine centers
iris.clusters$centers
#Add cluster to data
iris.z$clusters <- iris.clusters$cluster
#Clusterplot
library(cluster)
clusplot(iris.znorm2,iris.clusters$cluster,color = TRUE, shade = TRUE)
