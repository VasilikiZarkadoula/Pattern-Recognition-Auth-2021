cat("\014")
remove(list = ls())

library(cluster) 


# 1. k-Means

# create data
X = c(7, 3, 1, 5, 1, 7, 8, 5)
Y = c(1, 4, 5, 8, 3, 8, 2, 9)
rnames = c("x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8")
kdata = data.frame(X, Y, row.names = rnames)

plot(kdata, pch = 15)
text(kdata, labels = row.names(kdata), pos = 2)

model = kmeans(kdata, centers = kdata[1:3,])

plot(kdata, col = model$cluster, pch = 15)
text(kdata, labels = row.names(kdata), pos = 2)
points(model$centers, col = 1:length(model$centers), pch = "+", cex = 2)

# calculate cohesion and separation
cohesion = model$tot.withinss
separation = model$betweenss

################################################################################

# 2. k-Means and evaluation

# import data
cdata = read.csv("cdata.txt", stringsAsFactors = TRUE)
target = cdata[, 3]
cdata = cdata[, 1:2]

plot(cdata, col = target)

# choose the minimum number of clusters based on SSE
SSE = 1:10
for (i in 1:10){
  SSE[i] <- kmeans(cdata, centers = i)$tot.withinss
}
plot(1:10, SSE, type="b", xlab="Number of Clusters", ylab="SSE")

# apply k-Means for 3 clusters
model = kmeans(cdata, centers = 3)

# calculate cohesion and separation
cohesion = model$tot.withinss
separation = model$betweenss

plot(cdata, col = model$cluster)
points(model$centers, col = 4, pch = "+", cex = 2)

# calculate silhouette
model_silhouette = silhouette(model$cluster, dist(cdata))
plot(model_silhouette)
mean_silhouette = mean(model_silhouette[, 3])

cdata_ord = cdata[order(model$cluster),]
heatmap(as.matrix(dist(cdata_ord)), Rowv = NA, Colv = NA,col = heat.colors(256), revC = TRUE)





