#Individual Data mining assignment - Subashree Bhaskaran
#Clustering of the Engg Colleges - Engg_College_Data.csv

#Pre-requisite packages
#install.packages("factoextra", "cluster", "tidyverse")
library(factoextra)
library(cluster)
library(tidyverse)

#Read data & EDA
setwd("/Users/anand/Documents/BABI/DataMining/EnggCollege_Indiv_assignment")
data_engg <- read.csv("Engg_College_Data.csv")

str(data_engg)
data_engg$SR_NO <- as.factor(data_engg$SR_NO)
summary(data_engg)
#No missing data found / No scaling required

#As we have 26 observations, let us consider 2 - 4 clusters, 
#beyond which clusters will become too granular for analysis


#Calculate the Euclidean distance
data_engg_euc <- dist(data_engg[3:7], method = "euclidean")
#Visualize the distance matrix
fviz_dist(data_engg_euc, gradient = list(low = "#008000", mid = "white", high = "#FC4E07"))



#Do Clustering using Hierarchical with Average Linkage
clust_avg1 <- hclust(data_engg_euc, method = "average")
clust_avg1$height
#Visualize clusters with Dendogram for k=2
plot(clust_avg1, labels = data_engg$SR_NO)
rect.hclust(clust_avg1, k=2 , border = "red")

#Visualize clusters with Dendogram for k=3
plot(clust_avg1, labels = data_engg$SR_NO)
rect.hclust(clust_avg1, k=3 , border = "red")

#Visualize clusters with Dendogram for k=4
plot(clust_avg1, labels = data_engg$SR_NO)
rect.hclust(clust_avg1, k=4 , border = "red")


#Do Clustering with k-means
set.seed(1234)
#No change in data - hence, the same data_engg variable is used

#Perform k-means for k=2 & Visualize
clust_kmeans2 <- kmeans(data_engg[3:7], centers = 2, nstart = 25)
fviz_cluster(clust_kmeans2, data_engg[3:7])

#Perform k-means for k=3 & Visualize
clust_kmeans3 <- kmeans(data_engg[3:7], centers = 3, nstart = 25)
fviz_cluster(clust_kmeans3, data_engg[3:7])

#Perform k-means for k=4 & Visualize
clust_kmeans4 <- kmeans(data_engg[3:7], centers = 4, nstart = 25)
fviz_cluster(clust_kmeans4, data_engg[3:7])


#Perform k-means for k=8 & Visualize
clust_kmeans8 <- kmeans(data_engg[3:7], centers = 8, nstart = 25)
fviz_cluster(clust_kmeans8, data_engg[3:7])

# To know optimal k value: Compute average silhouette for k clusters

#Silhouette 
avg_sil <- function(k) {
  km.res <- kmeans(data_engg[3:7], centers = k, nstart = 25)
  ss <- silhouette(km.res$cluster, dist(data_engg[3:7]))
  mean(ss[, 3])
}

# Compute and plot wss for k = 2 to k = 6
k.values <- 2:16

# extract avg silhouette for 2-6 clusters
avg_sil_values <- map_dbl(k.values, avg_sil)

plot(k.values, avg_sil_values,
     type = "b", pch = 19, frame = FALSE, 
     xlab = "Number of clusters K",
     ylab = "Average Silhouettes")

#From average Silhouette, optimal k is 4 followed closely by 2
#k=5 has same silhouette value as that of 4. Hence, having 5 clusters is same as having 4.


#-----------------------------------------------
#Question: If the output differs then what linkage should have been used in Hierarchical
#so that the two output are almost matching.
#Answer: For  k=4 (optimal number of clusters), output is exacty same in both hierarchical & k-means. 
#Hence, there is no change in the linkage to be used.  
#However, I have used below different linkage to further substantiate my answer.
#-----------------------------------------------

clust_wardD <- hclust(data_engg_euc, method = "ward.D")

plot(clust_wardD, labels = data_engg$SR_NO)
rect.hclust(clust_wardD, k=4 , border = "red")

###
clust_wardD2 <- hclust(data_engg_euc, method = "ward.D2")

plot(clust_wardD2, labels = data_engg$SR_NO)
rect.hclust(clust_wardD2, k=4 , border = "red")
###

clust_single <- hclust(data_engg_euc, method = "single")

plot(clust_single, labels = data_engg$SR_NO)
rect.hclust(clust_single, k=4 , border = "red")
#####

clust_complete <- hclust(data_engg_euc, method = "complete")

plot(clust_complete, labels = data_engg$SR_NO)
rect.hclust(clust_complete, k=4 , border = "red")

#####
clust_mcquitty <- hclust(data_engg_euc, method = "mcquitty")

plot(clust_mcquitty, labels = data_engg$SR_NO)
rect.hclust(clust_mcquitty, k=4, border = "red")
#####

clust_median <- hclust(data_engg_euc, method = "median")

plot(clust_median, labels = data_engg$SR_NO)
rect.hclust(clust_median, k=4 , border = "red")
####

clust_centroid <- hclust(data_engg_euc, method = "centroid")

plot(clust_centroid, labels = data_engg$SR_NO)
rect.hclust(clust_centroid, k=4 , border = "red")


clust_avg8 <- hclust(data_engg_euc, method = "average")

plot(clust_centroid, labels = data_engg$SR_NO)
rect.hclust(clust_centroid, k=8 , border = "red")
