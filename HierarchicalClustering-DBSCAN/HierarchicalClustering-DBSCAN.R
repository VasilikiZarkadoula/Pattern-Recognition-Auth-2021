cat("\014")
remove(list = ls())

library(dbscan)
library(cluster)
library(scatterplot3d)

# 1. Hierarchical Clustering

# create data
X = c(2, 8, 0, 7, 6)
Y = c(0, 4, 6, 2, 1)
rnames = c("x1", "x2", "x3", "x4", "x5")
hdata = data.frame(X, Y, row.names = rnames)

plot(hdata, pch = 15)
text(hdata, labels = row.names(hdata), pos = 2)

# apply single linkage hierarchical clustering
d = dist(hdata)
hc_single = hclust(d, method = "single")
plot(hc_single)

# apply complete linkage hierarchical clustering
hc_complete = hclust(d, method = "complete")
plot(hc_complete)

# create two clusters
clusters = cutree(hc_single, k = 2)
plot(hdata, col = clusters, pch = 15, main = "Single Linkage")
text(hdata, labels = row.names(hdata), pos = 2)

clusters = cutree(hc_complete, k = 2)
plot(hdata, col = clusters, pch = 15, main = "Complete Linkage")
text(hdata, labels = row.names(hdata), pos = 2)


# import new data
europe = read.csv("europe.txt", stringsAsFactors = TRUE)

# apply complete linkage hierarchical clustering
d = dist(scale(europe)) # scale for normalization
hc = hclust(d, method = "complete")
plot(hc)

# select the number of clusters based on mean silhouette
slc = c()
for (i in 2:20){
  clusters = cutree(hc, k = i)
  slc [i-1] = mean(silhouette(clusters, d)[, 3])
}
plot(2:20, slc, type="b", xlab="Number of Clusters", ylab="Silhouette")
bestNumOfClusters = 1 + which(slc == max(slc))

clusters = cutree(hc, k = bestNumOfClusters)
plot(hc)
rect.hclust(hc, k = bestNumOfClusters)

s3d = scatterplot3d(europe, angle = 125, scale.y = 1.5, color = clusters)
coords <- s3d$xyz.convert(europe)
text(coords$x, coords$y, labels=row.names(europe), pos=sample(1:4), col = clusters)

model_silhouette = silhouette(clusters, d)
plot(model_silhouette)

# ################################################################################

# 2. Density-based Clustering

# create data
X = c(2, 2, 8, 5, 7, 6, 1, 4)
Y = c(10, 5, 4, 8, 5, 4, 2, 9)
rnames = c("x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8")
ddata = data.frame(X, Y, row.names = rnames)

plot(ddata, pch = 15)
text(ddata, labels = row.names(ddata), pos = 4)

# apply dbscan, eps = 2 and minPoints = 2
model = dbscan(ddata, eps = 2, minPts = 2)
clusters = model$cluster
plot(ddata, col=clusters+1, pch=15, main="DBSCAN (eps = 2, minPts = 2)")
text(ddata, labels = row.names(ddata), pos = 4)

# apply dbscan, eps = 3.5 and minPoints = 2
model = dbscan(ddata, eps = 3.5, minPts = 2)
clusters = model$cluster
plot(ddata, col=clusters+1, pch=15, main="DBSCAN(eps = 3.5, minPts = 2)")
text(ddata, labels = row.names(ddata), pos = 4)


# import new data
mdata = read.csv("mdata.txt", stringsAsFactors = TRUE)

plot(mdata)

# apply k-Means in order to create 2 clusters
model = kmeans(mdata, 2)
plot(mdata, col = model$cluster+1)

# choose eps for DBSCAN based on 10 knn distance
knndist = kNNdist(mdata, k = 10)
plot(sort(knndist), type = 'l', xlab = "Points sorted by distance", ylab = "10-NN distance")

model = dbscan(mdata, eps = 0.4, minPts = 10)
plot(mdata, col = model$cluster + 1, pch = ifelse(model$cluster, 1, 4))
