cat("\014")
remove(list = ls())

library(neuralnet)
library(class)

# 1. kNN model construction and  classification

# import data
knndata = read.csv("knndata.txt", stringsAsFactors = TRUE)

# split dataset
X_train = knndata[,c("X1","X2")] # or X_train = knndata[,c(1,2)]
Y_train = knndata$Y

plot(X_train, col = Y_train, pch = c("o","+")[Y_train])

# find class of observations (0.7, 0.4) and (0.7, 0.6)
knn1 = knn(X_train, c(0.7, 0.4), Y_train, k = 1, prob = TRUE)
knn2 = knn(X_train, c(0.7, 0.4), Y_train, k = 5, prob = TRUE)
knn3 = knn(X_train, c(0.7, 0.6), Y_train, k = 5, prob = TRUE)

########################################################################################

# 2. Perceptron model construction

# import data
anndata = data.frame(X1 = c(0,0,1,1), X2 = c(0,1,0,1), Y = c(1,1,-1,-1))

# split dataset
Xtrain = anndata[,c("X1","X2")]  # or Xtrain = anndata[,c(1,2)]
Ytrain = anndata$Y
Ytrain = ifelse(Ytrain > 0, 1, 2)

plot(Xtrain, col = Ytrain, pch = c("+","o")[Ytrain])

model = neuralnet(Y ~ X1 + X2, anndata, hidden = 0, threshold = 0.000001 ,startweights = c(0,0,0),learningrate = 1)
plot(model)
print(model$weights)

# #######################################################################################################

# 3. ANN model construction

# import data
alldata = read.csv("alldata.txt", stringsAsFactors = TRUE)

# split dataset into training and testing
trainingdata = alldata[1:600, ]
testdata = alldata[601:800, ]

plot(trainingdata[ ,c(1:2)], col = trainingdata$y, pch = c("+","o")[trainingdata$y])

model <- neuralnet(y ~ X1 + X2, data = trainingdata, hidden = c(2, 2), threshold = 0.01)
plot(model)

# Training Error
yEstimateTrain = compute(model, trainingdata[, c(1:2)])$net.result
TrainingError = trainingdata$y - yEstimateTrain
MAE = mean(abs(TrainingError))
plot(hist(TrainingError, breaks = 20))

# Testing Error
yEstimateTest = compute(model, testdata[, c(1:2)])$net.result
TestingError = testdata$y - yEstimateTest
MAE = mean(abs(TestingError))
plot(hist(TestingError, breaks = 20))
