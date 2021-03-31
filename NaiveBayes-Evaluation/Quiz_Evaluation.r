cat("\014")
rm(list = ls())

library(ROCR)
library(MLmetrics)

Class = c(1, 1, 0, 0, 1, 1, 0, 0, 1, 0)
P_M1 = c(0.73, 0.69, 0.44, 0.55, 0.67, 0.47, 0.08, 0.15, 0.45, 0.35)
P_M2 = c(0.61, 0.03, 0.68, 0.31, 0.45, 0.09, 0.38, 0.05, 0.01, 0.04)
data = data.frame(Class, P_M1, P_M2)

# 1) Calculate TPR and FPR for the 2 models
results = round(P_M1)
cm = ConfusionMatrix(results, Class)[2:1, 2:1]
tpr1 = cm[1,1]/( cm[1,1] + cm[1,2] )
fpr1 = cm[2,1]/( cm[2,1] + cm[2,2] )

results = round(P_M2)
cm = ConfusionMatrix(results, Class)[2:1, 2:1]
tpr2 = cm[1,1]/( cm[1,1] + cm[1,2] )
fpr2 = cm[2,1]/( cm[2,1] + cm[2,2] )

# second method to get results
# threshold = 0.5
# results = rep(0,length(P_M2))
# results[which(P_M2>threshold)] = 1;

# 2) Calculate F-measure for model 2
f2 = 2*cm[1,1]/(2*cm[1,1] + cm[1,2] + cm[2,1])
  # or
f2_2 = F1_Score(factor(Class), factor(results), positive = "1")


# 3) ROC curve and AUC
pred_obj1 = prediction(P_M1, Class)
ROCcurve1 = performance(pred_obj1, "tpr", "fpr")

plot(ROCcurve1,col = "blue")
par(new=TRUE)

pred_obj2 = prediction(P_M2, Class)
ROCcurve2 = performance(pred_obj2, "tpr", "fpr")
plot(ROCcurve2)

auc1 = performance(pred_obj1, "auc")
print(auc1@y.name)
print(auc1@y.values)

auc2 = performance(pred_obj2, "auc")
print(auc2@y.name)
print(auc2@y.values)
