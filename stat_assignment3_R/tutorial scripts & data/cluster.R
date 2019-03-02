#Cluster Analysis
library(tidyverse)
#Read in data
college.data <- read_csv("C:/Users/jonathan/Desktop/DAPT Docs/spring 2019/Statistics/stat_assignments_spring19_R/stat_assignment3_R/tutorial scripts & data/CollUniv.txt")
college.data$AccRate <- as.numeric(sub("%","",college.data$AccRate))
college.data$ExpPerStudent <- as.numeric(as.character(sub("\\$","",college.data$ExpPerStudent)))

#Hierarchical Clustering
d <- dist(college.data[,3:7],method="euclidean")
hier.clust <- hclust(d,method="ward.D")
plot(hier.clust)
groups <- cutree(hier.clust,k=5)
rect.hclust(hier.clust,k=5,border="red")
aggregate(college.data[,3:7],list(groups),FUN=mean)

#Two-way clustering
mydata <- as.matrix(college.data[,3:7])
rownames(mydata) <- college.data$School
datascaled <- scale(mydata)
heatmap(datascaled)

#K-means clustering
library(factoextra)
fviz_nbclust(college.data[,3:7],kmeans,method="gap_stat")
kmean.clust <- kmeans(college.data[,3:7],5)
aggregate(mydata,by=list(kmean.clust$cluster),FUN=mean)
new.data <- data.frame(college.data, kmean.clust$cluster)
fviz_cluster(kmean.clust,data=college.data[,3:7])

####################################################################
# Car Data

cars <- read_csv("C:/Users/jonathan/Desktop/DAPT Docs/spring 2019/Statistics/stat_assignments_spring19_R/stat_assignment3_R/tutorial scripts & data/cars_mod.txt")

#Hierarchical Clustering
d <- dist(cars[,3:16],method="euclidean")
hier.clust <- hclust(d,method="ward.D")
plot(hier.clust)
groups <- cutree(hier.clust,k=5)
rect.hclust(hier.clust,k=5,border="red")
aggregate(cars[,3:16],list(groups),FUN=mean)

#Two-way clustering
mydata <- as.matrix(cars[,3:16])
rownames(mydata) <- cars$make
datascaled <- scale(mydata)
heatmap(datascaled)

#K-means clustering
fviz_nbclust(na.omit(cars[,3:16]),kmeans,method="gap_stat")
kmean.clust <- kmeans(na.omit(cars[,3:16]),3)
aggregate(na.omit(cars[,3:16]),by=list(kmean.clust$cluster),FUN=mean)
new.data <- data.frame(na.omit(cars), kmean.clust$cluster)
fviz_cluster(kmean.clust,data=na.omit(cars[,3:16]))
