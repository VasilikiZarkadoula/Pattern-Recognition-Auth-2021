cat("\014")
remove(list = ls())

library(dbscan)
library(cluster)
library(MLmetrics)

# Find the best clustering method

# import data
dcdata = read.csv("dcdata.txt", stringsAsFactors = TRUE)
target = dcdata[, 3]
dcdata = dcdata[, 1:2]
plot(dcdata,col = target)

# a 
d = dist(dcdata)

hc_single = hclust(d, method = "single")
clusters = cutree(hc_single, k = 2)
plot(dcdata, col = clusters + 1, main = "Single Linkage")
acc1 = Accuracy(clusters,target)

# b 
hc_complete = hclust(d, method = "complete")
clusters = cutree(hc_complete, k = 2)
plot(dcdata, col = clusters + 1, main = "Complete Linkage")
acc2 = Accuracy(clusters,target)

# c
model = dbscan(dcdata, eps = 0.75, minPts = 5)
plot(dcdata, col = model$cluster + 1, pch = ifelse(model$cluster, "o", "x"), main = "DBSCAN (eps = 0.75)")

model = dbscan(dcdata, eps = 1.0, minPts = 5)
plot(dcdata, col = model$cluster + 1, pch = ifelse(model$cluster, "o", "x"), main = "DBSCAN (eps = 1.0)")

model = dbscan(dcdata, eps = 1.25, minPts = 5)
plot(dcdata, col = model$cluster + 1, pch = ifelse(model$cluster, "o", "x"), main = "DBSCAN (eps = 1.25)")

model = dbscan(dcdata, eps = 1.5, minPts = 5)
plot(dcdata, col = model$cluster + 1, pch = ifelse(model$cluster, "o", "x"), main = "DBSCAN (eps = 1.5)")

# d
model = kmeans(dcdata, 2)
plot(dcdata, col = model$cluster + 1)



