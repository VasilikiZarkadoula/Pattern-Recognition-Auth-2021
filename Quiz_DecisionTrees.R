cat("\014")
remove(list = ls())

library(rpart)
library(rpart.plot)

debugSource("Gini.R")
debugSource("OverallGini.R")
debugSource("InfoGain.R")
debugSource("EntropyAll.R")

data = read.csv('data.csv', sep = ';', header = TRUE, stringsAsFactors = TRUE)

#1)
GiniAll = OverallGini(data,5)

#2)
GiniCustomerID = Gini(data,1,5)

#3)
GiniSex = Gini(data,2,5)

#4)
GiniCarType = Gini(data,3,5)

#5)
GiniBudget = Gini(data,4,5)

#7)
model <- rpart(Insurance ~ Budget + Sex + CarType, method = "class", data = data, minsplit = 1, minbucket = 1, cp = -1)
rpart.plot(model, extra = 104, nn = TRUE)
  

