
library(factoextra)
library(cluster)
library(fpc)

rm(list=ls())


setwd("/Users/jayavarshini/Desktop/ms/sem1/dm/Mydata")

data <- read.csv("lang.csv", sep=",", header=T,comment.char = "#",row.names=1)

#dontscale the data
#df<-scale(data)

# a) Run hierarchical clustering on the dataset using factoextra::eclust() method. Run the clustering algorithm for three linkages: single, complete, and average. Plot the dendogram associated with each linkage using fviz_dend().

hc.single <- eclust(data, "hclust",hc_method = "single")
fviz_dend(hc.single,show_labels = TRUE,pallete="jco",as.ggplot=T)
#{Great Britain, Ireland},{France,Belgium},{Luxemburg,Switzerland},{Denmark, Norway}{Austria,WestGermany}
hc.complete <- eclust(data, "hclust",hc_method = "complete")
fviz_dend(hc.complete,show_labels = TRUE,pallete="jco",as.ggplot=T)
#{Great Britain, Ireland},{France,Belgium},{Luxemburg,Switzerland},{Denmark, Norway}{Austria,WestGermany}
hc.average <- eclust(data, "hclust",hc_method = "average")
fviz_dend(hc.average,show_labels = TRUE,pallete="jco",as.ggplot=T)
#{spain,portugal}{Great Britain, Ireland},{France,Belgium},{Luxemburg,Switzerland},{Denmark, Norway}{Austria,WestGermany}
#c)Italy is clustered with the large cluster in the average and single linkage and with a samaller one in complete linkage. Italy must be clustered with the large cluster  
# In italy, People speak italy and few speak french which is why it is clustered with france and german in complete linkage


#d) Pure cluster: Average which has 6 two-singleton clusters and the single,complete linakge two have 5

#e)
cluster<-cutree(hc.average,h=125)
table(cluster)
#there are 7 clusters.

#f) Use k parameter
hc.kcomplete <- eclust(data, "hclust",k=7,hc_method = "complete")
fviz_dend(hc.kcomplete,show_labels = TRUE,pallete="jco",as.ggplot=T)
hc.ksingle <- eclust(data, "hclust",k=7,hc_method = "single")
fviz_dend(hc.ksingle,show_labels = TRUE,pallete="jco",as.ggplot=T)
hc.kavg <- eclust(data, "hclust",k=7,hc_method = "average")
fviz_dend(hc.kavg,show_labels = TRUE,pallete="jco",as.ggplot=T)
#g)print the Dunn and Silhouette width using the fpc::cluster.stats() method

stats.single<-cluster.stats(dist(data), hc.ksingle$cluster)
stats.single$dunn
stats.single$avg.silwidth

stats.complete<-cluster.stats(dist(data), hc.kcomplete$cluster)
stats.complete$dunn
stats.complete$avg.silwidth

stats.average<-cluster.stats(dist(data), hc.kavg$cluster)
stats.average$dunn
stats.average$avg.silwidth
#h)Average linkage 
#i)complete linkage



