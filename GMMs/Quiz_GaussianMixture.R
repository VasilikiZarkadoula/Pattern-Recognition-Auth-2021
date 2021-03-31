cat("\014")
remove(list = ls())

library(cluster)
library(mixtools)

# Compare k-means and GMMs clustering

kmdata = read.csv("kmdata.txt", stringsAsFactors = TRUE)
x = kmdata[, 1:2]
y = kmdata[, 3]

# 1.
plot(x)
plot(x, col = y)

# 2.
model = kmeans(x, 3)
plot(x, col = model$cluster)
points(model$centers, col = 5, pch = "+", cex = 2)

# 3.
model2 = mvnormalmixEM(x, k = 3 , epsilon = 0.01)
plot(model2, which = 2) 

