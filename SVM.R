cat("\014")
remove(list = ls())

library(e1071)
library(MLmetrics)

# Construction of SVM Model and Application with Cross-validation

alldata = read.csv("alldata.txt", stringsAsFactors = TRUE)
trainingdata = alldata[1:600, ]
testdata = alldata[601:800, ]

xtrain = trainingdata[c(1,2)]
ytrain = trainingdata$y

# a)
plot(xtrain,col = ytrain, pch = c("o","+")[ytrain])

# b) Apply SVM with RBF kernel and gamma = 1, gamma = 0.01 and gamma = 100
X1 = seq(min(trainingdata[, 1]), max(trainingdata[, 1]), by = 0.1)
X2 = seq(min(trainingdata[, 2]), max(trainingdata[, 2]), by = 0.1)
mygrid = expand.grid(X1, X2)
colnames(mygrid) = colnames(trainingdata)[1:2]

#gamma = 1
svm_model = svm(y ~ ., kernel="radial", type="C-classification",data = trainingdata, gamma = 1)
pred = predict(svm_model, mygrid)
Y = matrix(pred, length(X1), length(X2))
contour(X1, X2, Y, add = TRUE, levels = 1.5, labels = "gamma = 1", col = "blue")

#gamma = 0.01
svm_model = svm(y ~ ., kernel="radial", type="C-classification",data = trainingdata, gamma = 0.01)
pred = predict(svm_model, mygrid)
Y = matrix(pred, length(X1), length(X2))
contour(X1, X2, Y, add = TRUE, levels = 1.5, labels = "gamma = 0.01", col = "red")

#gamma = 100
svm_model = svm(y ~ ., kernel="radial", type="C-classification",data = trainingdata, gamma = 100)
pred = predict(svm_model, mygrid)
Y = matrix(pred, length(X1), length(X2))
contour(X1, X2, Y, add = TRUE, levels = 1.5, labels = "gamma = 100", col = "green")

#c) For gamma in gammavalues, calculate the training and testing error
gammavalues = c(0.01, 0.1, 1, 10, 100, 1000, 10000, 100000, 1000000)

training_error = c()
for (gamma in gammavalues) {
  svm_model = svm(y ~ ., kernel="radial", type="C-classification",data = trainingdata, gamma = gamma)
  pred = predict(svm_model, trainingdata[, c(1:2)])
  training_error = c(training_error, 1 - Accuracy(trainingdata$y, pred))
}

testing_error = c()
for (gamma in gammavalues) {
  svm_model = svm(y ~ ., kernel="radial", type="C-classification",data = trainingdata, gamma = gamma)
  pred = predict(svm_model, testdata[, c(1:2)])
  testing_error = c(testing_error, 1 - Accuracy(testdata$y, pred))
}

# plot errors 
plot(training_error, type = "l", col="blue", ylim = c(0, 0.5), xlab = "Gamma", ylab = "Error", xaxt = "n")
axis(1, at = 1:length(gammavalues), labels = gammavalues)
lines(testing_error, col="red")
legend("right", c("Training Error", "Testing Error"), pch = c("-","-"), col = c("blue", "red"))


#e) Apply 10-fold cross validation to find the best value for gamma
k = 10
dsize = nrow(trainingdata)
folds = split(sample(1:dsize), ceiling(seq(dsize) * k / dsize))

accuracies <- c()
for (gamma in gammavalues) {
  predictions <- data.frame()
  testsets <- data.frame()
  for(i in 1:k){
    # Select 9 out of 10 folds for training and 1 for validation
    trainingset <- trainingdata[unlist(folds[-i]),]
    validationset <- trainingdata[unlist(folds[i]),]
    # Train and apply the model
    svm_model = svm(y ~ ., kernel="radial", type="C-classification",data = trainingset, gamma = gamma)
    pred = predict(svm_model, validationset[, c(1:2)])
    # Save predictions and testsets
    predictions <- rbind(predictions, as.data.frame(pred))
    testsets <- rbind(testsets, as.data.frame(validationset[,3]))
  }
  # Calculate the new accuracy and add it to the previous ones
  accuracies = c(accuracies, Accuracy(predictions, testsets))
}

bestgamma = gammavalues[which.max(accuracies)]
