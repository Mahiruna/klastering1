#K-means using R
#Source: http://rpubs.com/Nitika/kmeans_Iris

#Load and view dataset
require("datasets")
data("iris") #load Iris Dataset
str(iris) #view structure of dataset
summary(iris) #view statistical summary of dataset
head(iris) #view top  rows of dataset

#Preprocess the dataset
#Since clustering is a type of Unsupervised Learning, 
#we would not require Class Label(output) during execution of our algorithm. 
#We will, therefore, remove Class Attribute "Species" and store it in another variable. 
#We would then normalize the attributes between 0 and 1 using our own function.

iris.new<- iris[,c(1,2,3,4)] #membuat data iris baru tanpa label
iris.class<- iris[,"Species"] #menentukan nama label dataset
head(iris.new)

#Membuat function Normalisasi data dengan minmax normalization
normalize <- function(x){
  return ((x-min(x))/(max(x)-min(x)))
}

iris.new$Sepal.Length<- normalize(iris.new$Sepal.Length)
iris.new$Sepal.Width<- normalize(iris.new$Sepal.Width)
iris.new$Petal.Length<- normalize(iris.new$Petal.Length)
iris.new$Petal.Width<- normalize(iris.new$Petal.Width)
head(iris.new)

#Apply k-means clustering algorithm
start <- matrix(c(5.19, 6.40, 5.12, 3.62, 2.95, 2.80, 1.49, 5.12, 2.54, 0.27, 1.79, 0.62), 3, 4)
result<- kmeans(iris.new,centers = start) #aplly k-means algorithm with no. of centroids(k)=3
result$size # gives no. of records in each cluster
result$centers # gives value of cluster center datapoint value(3 centers for k=3)
result$cluster #gives cluster vector showing the custer where each record falls
cluster.output <- cbind(iris.new ,result$cluster) #membuat matriks hasil cluster
write.csv(cluster.output, file = "km clusters.csv", row.names = TRUE)

#Verify results of clustering
par(mfrow=c(2,2), mar=c(5,4,2,2))
plot(iris.new[c(1,2)], col=result$cluster)# Plot to see how Sepal.Length and Sepal.Width data points have been distributed in clusters
plot(iris.new[c(1,2)], col=iris.class)# Plot to see how Sepal.Length and Sepal.Width data points have been distributed originally as per "class" attribute in dataset
plot(iris.new[c(3,4)], col=result$cluster)# Plot to see how Petal.Length and Petal.Width data points have been distributed in clusters
plot(iris.new[c(3,4)], col=iris.class)

table(result$cluster,iris.class) #membuat confusion matrik

#menghitung nilai DBI
#https://rdrr.io/cran/clusterSim/man/index.DB.html
library(cluster)
library(MASS)
library(clusterSim)
index.DB(iris.new, result$cluster, p=2, q=1)

library(NbClust)
NbClust(data = iris.new, diss = NULL, distance = "euclidean", min.nc = 2, max.nc = 5, method = "complete", index = "all", alphaBeale = 0.1)

#Rand Index
#https://davetang.org/muse/2017/09/21/the-rand-index/
library(sp)
library(maps)
library(foreign)
library(shapefiles)
library(fossil)
true_label <- as.numeric(iris$Species)
true_label
rand.index(true_label, result$cluster)

