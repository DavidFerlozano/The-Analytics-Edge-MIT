data = read.csv("Households.csv")

length(which(data$MorningPct==100))
length(which(data$AfternoonPct==100))

sub1 = subset(data, data$AvgSalesValue > 150)
sub2 = subset(data, data$AvgDiscount > 25)

length(which(data$NumVisits>=300))/2500

library(caret)
preproc = preProcess(data)
HouseholdsNorm = predict(preproc, data)

set.seed(200)
distances <- dist(HouseholdsNorm, method = "euclidean")
ClusterShoppers <- hclust(distances, method = "ward.D")
plot(ClusterShoppers, labels = FALSE)

set.seed(200)
clusterGroups = kmeans(HouseholdsNorm, centers = 10)
Cluster = split(HouseholdsNorm, clusterGroups$cluster)
cluster1 = Cluster[[1]]
cluster2 = Cluster[[2]]
cluster3 = Cluster[[3]]
cluster4 = Cluster[[4]]
cluster5 = Cluster[[5]]
cluster6 = Cluster[[6]]
cluster7 = Cluster[[7]]
cluster8 = Cluster[[8]]
cluster9 = Cluster[[9]]
cluster10 = Cluster[[10]]

tail(sort(colMeans(cluster1)))
tail(sort(colMeans(cluster2)))
tail(sort(colMeans(cluster3)))
tail(sort(colMeans(cluster4)))
tail(sort(colMeans(cluster5)))
tail(sort(colMeans(cluster6)))
tail(sort(colMeans(cluster7)))
tail(sort(colMeans(cluster8)))
tail(sort(colMeans(cluster9)))
tail(sort(colMeans(cluster10)))

set.seed(5000)
clusterGroups = kmeans(HouseholdsNorm, centers = 5)
Cluster = split(HouseholdsNorm, clusterGroups$cluster)
cluster1 = Cluster[[1]]
cluster2 = Cluster[[2]]
cluster3 = Cluster[[3]]
cluster4 = Cluster[[4]]
cluster5 = Cluster[[5]]

tail(sort(colMeans(cluster1)))
tail(sort(colMeans(cluster2)))
tail(sort(colMeans(cluster3)))
tail(sort(colMeans(cluster4)))
tail(sort(colMeans(cluster5)))
