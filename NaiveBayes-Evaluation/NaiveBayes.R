cat("\014")
remove(list = ls())

library(e1071)
library(MLmetrics)
library(ROCR)


# 1. Naive Bayes Model Construction and Classification

# import data
traffic = read.csv("traffic.txt", stringsAsFactors = TRUE)

# construct Naive Bayes Model with and without Laplace smoothing
model <- naiveBayes(as.factor(HighTraffic) ~ ., data = traffic)
#model <- naiveBayes(as.factor(HighTraffic) ~ ., data = traffic,laplace = 1)

# find the class of obseravtion (Weather, Day) = (Hot, Vacation)
trvalue <- data.frame(Weather = factor("Hot", levels(as.factor(traffic$Weather))), Day = factor("Vacation", levels(as.factor(traffic$Day))))
pred = predict(model, trvalue,type = "raw")
if (pred[1]>pred[2]){
  classHotVacation = 1;
}else{
  classHotVacation = 2;
}

# find the class of obseravtion (Weather, Day) (Weather, Day) = (Hot, Weekend)
trvalue <- data.frame(Weather = factor("Hot", levels(as.factor(traffic$Weather))), Day = factor("Weekend", levels(as.factor(traffic$Day))))
pred = predict(model, trvalue, type = "raw")
if (pred[1]>pred[2]){
  classHotWeekend = 1;
}else{
  classHotWeekend = 2;
}

################################################################################

# 2. Application with Evaluation Metrics and ROC Curve

# load data
data(HouseVotes84, package = "mlbench")
votes = na.omit(HouseVotes84) # ignore missing values

# split dataset into training and testing 
trainingdata = votes[1:180,]
testingdata = votes[181:232,]

# construct Naive Bayes model using the trainig data
model <- naiveBayes(as.factor(Class) ~ ., data = trainingdata)

# apply the model to testingdata
xtest = testingdata[,-1]  
ytest = testingdata[,1]  

ypred = predict(model, xtest)
predprob = predict(model, xtest, type = "raw")

# for testingdata calculate precision, recall and f-measure for the democrat class
cm = ConfusionMatrix(ypred, ytest)
pr = Precision(ytest, ypred, "democrat")
re = Recall(ytest, ypred, "democrat")
f1 = 2 * pr * re/ (pr + re)

# ROC curve
pred_obj = prediction(predprob[,1], ytest, label.ordering = c("republican", "democrat"))
ROCcurve <- performance(pred_obj, "tpr", "fpr")

plot(ROCcurve, col = "blue")
abline(0,1, col = "grey")

# area under curve (auc)
aucObject = performance(pred_obj, "auc")
auc = aucObject@y.values



