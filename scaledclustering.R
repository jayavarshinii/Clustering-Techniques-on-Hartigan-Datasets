library(cluster)

library(factoextra)

#Display the existing directory
print(getwd())

#Change to the current wokring directory and display the new location
setwd("/Users/jayavarshini/Desktop/ms/sem1/dm/Mydata")
rm(list=ls())
# Create a object  
mammal <- read.table('harington.txt', header=TRUE,skip=20)
print(mammal)

#a)i)#names are the ones in characters, considering that I removed it from the data set and performed clustering with the other attributes.
mammal<- mammal[,-1]

#a)ii) the data is standardized. 
mammalscaled <-scale(mammal)

#a)iii)
write.table(mammal,file="Mammal.txt")
write.table(mammalscaled,file="mammalscaled.txt")
write.csv(mammalscaled,file = "mms.csv")
#b)i)

fviz_nbclust(mammalscaled, kmeans, method="silhouette")



#b)ii)
k <- kmeans(mammalscaled, centers=8,nstart=25) 
fviz_cluster(kmeans(mammalscaled, centers=1, nstart=25), data=mammal[1:8])
fviz_cluster(kmeans(mammalscaled, centers=2, nstart=25), data=mammal[1:8])
fviz_cluster(kmeans(mammalscaled, centers=3, nstart=25), data=mammal[1:8])
fviz_cluster(kmeans(mammalscaled, centers=4, nstart=25), data=mammal[1:8])
fviz_cluster(kmeans(mammalscaled, centers=5, nstart=25), data=mammal[1:8])
fviz_cluster(kmeans(mammalscaled, centers=6, nstart=25), data=mammal[1:8])
fviz_cluster(kmeans(mammalscaled, centers=7, nstart=25), data=mammal[1:8])
fviz_cluster(kmeans(mammalscaled, centers=8, nstart=25), data=mammal[1:8])



#b)iii) How many observations are in each cluster 
k$size

#b)iv)ktotal SSE of clusters
k$withinss

#b)v) SSE of each clusters
k$tot.withinss

#b)vi)
which(k$cluster == 1)
which(k$cluster == 2)
which(k$cluster == 3)
which(k$cluster == 4)
which(k$cluster == 5)
which(k$cluster == 6)
which(k$cluster == 7)
which(k$cluster == 8)





