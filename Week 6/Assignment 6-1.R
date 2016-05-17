dailykos <- read.csv("C:/Users/JaroD/Desktop/New Start/MOOC Courses/The Analytics Edge - MIT/Week 6/dailykos.csv")

distances = dist(dailykos, method = "euclidean")
clusterdailykos = hclust(distances, method = "ward.D")

plot(clusterdailykos)

clusterGroups = cutree(clusterdailykos, k = 7)
Cluster = split(dailykos, clusterGroups)
cluster1 = Cluster[[1]]
cluster2 = Cluster[[2]]
cluster3 = Cluster[[3]]
cluster4 = Cluster[[4]]
cluster5 = Cluster[[5]]
cluster6 = Cluster[[6]]
cluster7 = Cluster[[7]]

tail(sort(colMeans(cluster1)))

tail(sort(colMeans(cluster2)))
tail(sort(colMeans(cluster3)))
tail(sort(colMeans(cluster4)))
tail(sort(colMeans(cluster5)))
tail(sort(colMeans(cluster6)))
tail(sort(colMeans(cluster7)))

set.seed(1000)
clusterGroups_Kmeans = kmeans(dailykos, centers=7)
cluster_kmeans = split(dailykos, clusterGroups_Kmeans$cluster)
cluster1_kmeans = cluster_kmeans[[1]]
cluster2_kmeans = cluster_kmeans[[2]]
cluster3_kmeans = cluster_kmeans[[3]]
cluster4_kmeans = cluster_kmeans[[4]]
cluster5_kmeans = cluster_kmeans[[5]]
cluster6_kmeans = cluster_kmeans[[6]]
cluster7_kmeans = cluster_kmeans[[7]]

tail(sort(colMeans(cluster1_kmeans)))
tail(sort(colMeans(cluster2_kmeans)))
tail(sort(colMeans(cluster3_kmeans)))
tail(sort(colMeans(cluster4_kmeans)))
tail(sort(colMeans(cluster5_kmeans)))
tail(sort(colMeans(cluster6_kmeans)))
tail(sort(colMeans(cluster7_kmeans)))

table(clusterGroups, clusterGroups_Kmeans$cluster)
