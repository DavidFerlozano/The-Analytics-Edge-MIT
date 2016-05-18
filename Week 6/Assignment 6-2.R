airlines <- read.csv("C:/Users/JaroD/Desktop/New Start/MOOC Courses/The Analytics Edge - MIT/Week 6/AirlinesCluster.csv")

summary(airlines)

#pre-processes the data & performs the normalization
library(caret)
preproc = preProcess(airlines)
airlinesNorm = predict(preproc, airlines)
summary(airlinesNorm)

distances = dist(airlinesNorm, method = "euclidean")
clusterairlines = hclust(distances, method = "ward.D")
plot(clusterairlines)

clusterGroups = cutree(clusterairlines, k = 5)
Cluster = split(airlinesNorm, clusterGroups)
cluster1 = Cluster[[1]]
cluster2 = Cluster[[2]]
cluster3 = Cluster[[3]]
cluster4 = Cluster[[4]]
cluster5 = Cluster[[5]]

lapply(split(airlines, clusterGroups), colMeans)

set.seed(88)
clusterGroups_Kmeans = kmeans(airlinesNorm, centers=5,iter.max=1000)
cluster_kmeans = split(airlinesNorm, clusterGroups_Kmeans$cluster)
cluster1_kmeans = cluster_kmeans[[1]]
cluster2_kmeans = cluster_kmeans[[2]]
cluster3_kmeans = cluster_kmeans[[3]]
cluster4_kmeans = cluster_kmeans[[4]]
cluster5_kmeans = cluster_kmeans[[5]]

