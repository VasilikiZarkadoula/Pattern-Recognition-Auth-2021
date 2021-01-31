cat("\014")
remove(list = ls())


library(vegan)  
library(scatterplot3d)


# 1. Preprocessing

# import data
engdata = read.csv("engdata.txt", stringsAsFactors = TRUE)
pdata = engdata[, 1:2]

# delete duplicates from data.
pdata = unique(pdata)
plot(pdata)

# subtract the mean and divide by the standard deviation
transformed = scale(pdata, center = TRUE, scale = TRUE)
plot(transformed)

# apply sampling with replacement
sampdata = pdata[sample(nrow(pdata), 150, replace = TRUE),]
plot(sampdata)

# apply data discretization
discAge = cut(pdata$Age, seq(0,80,10))
discSalary = cut(pdata$Salary, seq(0,4000,400), dig.lab = 4)

plot(discAge)
plot(discSalary, las=2)


################################################################################

# 2. PCA

# import data
pdata = data.frame(X = c(1,0,-1,0,-1,1), Y = c(0,1,1,-1,0,-1), Z = c(-1,-1,0,1,1,0))
row.names(pdata) <- c("x1", "x2", "x3", "x4", "x5", "x6")

# 3d plot
s3d = scatterplot3d(pdata, color = "green", pch = 19, scale.y = 1.5)
coords <- s3d$xyz.convert(pdata)
text(coords$x, coords$y, labels=row.names(pdata), pos=2)

pca_model <- prcomp(pdata, center = TRUE, scale = TRUE)
eigenvalues = pca_model$sdev^2
eigenvectors = pca_model$rotation
pdata_pc <- as.data.frame(predict(pca_model, pdata)[, 1:2])
plot(pdata_pc)

# import new data
engdata = read.csv("engdata.txt", stringsAsFactors = TRUE)
Location = engdata[, 5]
engdata = engdata[, 1:4]

plot(engdata, col = Location, pch = c("o", "+")[Location])

# calculate the correlation matrix
corMatr =  cor(engdata)

# construct a PCA model and find the eigenvalues and eigenvectors
pca_model <- prcomp(engdata, center = TRUE, scale = TRUE)
eigenvalues = pca_model$sdev^2
eigenvectors = pca_model$rotation
barplot(eigenvalues / sum(eigenvalues))

# apply the model to data and maintain the first two dimensions
engdata_pc <- as.data.frame(predict(pca_model, engdata)[, 1:2])
plot(engdata_pc, col = Location, pch = c("o", "+")[Location])

# reconstruct the data and calculate the loss of information
engdata_pc[, 3:4] <- 0
engdata_rec = data.frame(t(t(as.matrix(engdata_pc) %*% t(pca_model$rotation)) * pca_model$scale+ pca_model$center))
plot(engdata_rec, col = Location, pch = c("o", "+")[Location])

info_loss = (eigenvalues[3] + eigenvalues[4]) / sum(eigenvalues)

# ################################################################################

# 3. ISOMAP

# import data
srdata = read.csv("srdata.txt", stringsAsFactors = TRUE)

# 3d plot
scatterplot3d(srdata, angle = 88, scale.y = 5)

# apply the ISOMAP algorithm with k = 4 to transform the data into two dimensions
srdata_dist = dist(srdata)
isom = isomap(srdata_dist, ndim=2, k = 4)
srdata_2d = isom$points

colors = srdata_2d[,1] -  min(srdata_2d[,1]) + 1
scatterplot3d(srdata, angle = 88, scale.y = 5, color = colors)
plot(srdata_2d, col = colors)

