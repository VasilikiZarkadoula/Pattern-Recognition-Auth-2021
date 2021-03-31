cat("\014")
remove(list = ls())

library(cluster)

sdata = read.csv("sdata.txt", stringsAsFactors = TRUE)

c1 = c(-4,10)
c2 = c(0,0)
c3 = c(4,10)
centers = rbind(c1,c2,c3)
model = kmeans(sdata, centers = centers)

# 1.
cohesion = model$tot.withinss

# 2.
separation = model$betweenss

# 3.
model_silhouette = silhouette(model$cluster, dist(sdata))
mean_silhouette = mean(model_silhouette[, 3])

# 4
c4 = c(-2,0)
c5 = c(2,0)
c6 = c(0,10)
centers2 = rbind(c4,c5,c6)
model2 = kmeans(sdata, centers = centers2)

plot(sdata, col = model2$cluster)
points(model2$centers, col = 4, pch = "+", cex = 2)

cohesion2 = model2$tot.withinss
separation2 = model2$betweenss

model_silhouette2 = silhouette(model2$cluster, dist(sdata))
mean_silhouette2 = mean(model_silhouette2[, 3])

