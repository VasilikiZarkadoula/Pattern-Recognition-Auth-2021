cat("\014")
remove(list = ls())

library(vegan)
library(scatterplot3d)
library(class)
library(MLmetrics)

# import data
data(Glass, package = "mlbench")

training = Glass[c(1:50, 91:146), -10] 
trainingType = factor(Glass[c(1:50, 91:146), 10]) 

testing = Glass[51:90, -10] 
testingType = factor(Glass[51:90, 10]) 

# 1. 
pca_model <- prcomp(training, center = TRUE, scale = TRUE)
eigenvalues = pca_model$sdev^2

result1 = eigenvalues[1]/sum(eigenvalues)

# 2.
result2 = sum(eigenvalues[5:9])/sum(eigenvalues)

# 3.
ypredKnn = knn(training, testing, trainingType, k = 3, prob = TRUE)
result3 = Accuracy(ypredKnn,testingType)

# 4.
result4 = Recall(testingType,ypredKnn,2)

# 5.
accuracies_pc = 1:9

pca_model <- prcomp(training, center = TRUE, scale = TRUE)
barplot(eigenvalues / sum(eigenvalues))

for (i in 1:9){
  training_pc = as.data.frame(predict(pca_model, training)[, 1:i])
  testing_pc = as.data.frame(predict(pca_model, testing)[, 1:i])

  ypredKnn = knn(training_pc, testing_pc, trainingType, k = 3, prob = TRUE)
  accuracies_pc[i] = Accuracy(ypredKnn,testingType)

}
print(accuracies_pc)
result5 = which(accuracies_pc==max(accuracies_pc))