cat("\014")
remove(list = ls())

library(neuralnet)
library(class)

X1 = c(-2.0, -2.0, -1.8, -1.4, -1.2, 1.2, 1.3, 1.3, 2.0, 2.0, -0.9, -0.5, -0.2, 0.0, 0.0, 0.3, 0.4, 0.5, 0.8, 1.0)
X2 = c(-2.0, 1.0, -1.0, 2.0, 1.2, 1.0, -1.0, 2.0, 0.0, -2.0, 0.0, -1.0, 1.5, 0.0, -0.5, 1.0, 0.0, -1.5, 1.5, 0.0)
Y = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2)
alldata = data.frame(X1, X2, Y)
xtrain = alldata[,c("X1","X2")]
ytrain = alldata$Y

# 0)
plot(xtrain, col = ytrain, pch = c("o","+")[ytrain])

# 1) find class of (1.5,-0.5) for k=3
knn1 = knn(xtrain, c(1.5,-0.5), ytrain, k = 3, prob = TRUE)
print(knn1[1])

# 2) find class of (-1,1) for k=5
knn2 = knn(xtrain, c(-1,1), ytrain, k = 5, prob = TRUE)
print(knn2)

# 3)

# import new data
X1 = c(2,2,-2,-2,1,1,-1,-1)
X2 = c(2,-2,-2,2,1,-1,-1,1)
Y = c(1,1,1,1,2,2,2,2)
alldata = data.frame(X1, X2, Y)
xtrain = alldata[,c("X1","X2")]
ytrain = alldata$Y

knn3 = knn(xtrain, c(-2,0.5), ytrain, k = 3, prob = TRUE)
print(knn3)

model <- neuralnet(Y ~ X1 + X2, data = alldata, hidden = c(2), threshold = 0.01)
plot(model)

#Error
yEstimateTest = compute(model, xtrain)$net.result
TestingError = ytrain - yEstimateTest
MAE = mean(abs(TestingError))
plot(hist(TestingError, breaks = 20))
print(MAE)


