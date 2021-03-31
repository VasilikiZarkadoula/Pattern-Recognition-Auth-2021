cat("\014")
remove(list = ls())

library(e1071)
library(MLmetrics)
library(class)

# import data
X1 = c(2,2,-2,-2,1,1,-1,-1)
X2 = c(2,-2,-2,2,1,-1,-1,1)
Y = c(1,1,1,1,2,2,2,2)
alldata = data.frame(X1, X2, Y)
xtrain = alldata[, c(1:2)]
ytrain = alldata$Y

plot(xtrain, col = ytrain, pch = c("o","+")[ytrain])

# 1) find class of observasion (4, 5) when k=1
knnClassif1 = knn(xtrain, c(4, 5), ytrain, k = 1, prob = TRUE)
print(knnClassif1)

# 2) find class of observasion (1.8, 4) when k=3
knnClassif2 = knn(xtrain, c(1.8, 4), ytrain, k = 3, prob = TRUE)
print(knnClassif2)

# 3) construct SVM model with RBF kernel and gamma = 1, calculate accuracy for the trainig set
svm_model = svm(Y ~ ., kernel="radial", type="C-classification",data = alldata, gamma = 1)
ypred = predict(svm_model, xtrain)
acc = Accuracy(ytrain, ypred)

# 4) construct SVM model with RBF kernel and gamma = 1000000 and classify observation (-2,-2.9)
svm_model = svm(Y ~ ., kernel="radial", type="C-classification",data = alldata, gamma = 1000000)
query = data.frame( "X1" = -2, "X2" = -1.9 )
qpred = predict(svm_model, query)

X1 = seq(min(alldata[, 1]), max(alldata[, 1]), by = 0.1)
X2 = seq(min(alldata[, 2]), max(alldata[, 2]), by = 0.1)
mygrid = expand.grid(X1, X2)
colnames(mygrid) = colnames(alldata)[1:2]
pred = predict(svm_model, mygrid)
Y = matrix(pred, length(X1), length(X2))

contour(X1, X2, Y, add = TRUE, levels = 1.5, col = "red")
