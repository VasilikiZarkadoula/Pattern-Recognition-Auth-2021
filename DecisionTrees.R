cat("\014")
remove(list = ls())

library(rpart)
library(rpart.plot)

# debugSource("Gini.R")
# debugSource("OverallGini.R")
# debugSource("InfoGain.R")
# debugSource("EntropyAll.R")


# import data
weather = read.csv("weather.txt", stringsAsFactors = TRUE)


# 1. decision tree + plot

# split based on Outlook
model <- rpart(Play ~ Outlook, method = "class", data = weather, minsplit = 1)
rpart.plot(model, extra = 104, nn = TRUE)
# split based on the entire dataset
model <- rpart(Play ~ Outlook + Temperature + Humidity, method = "class", data = weather, minsplit = 1, minbucket = 1, cp = -1)
rpart.plot(model, extra = 104, nn = TRUE)

# 2 GINI + INFORMATION GAIN

# GINI
  # Outlook
absfreq = table(weather[, c(1, 4)])
freq = prop.table(absfreq, 1)
freqSum = rowSums(prop.table(absfreq))
GINI_Sunny = 1 - freq["Sunny", "No"]^2 - freq["Sunny", "Yes"]^2
GINI_Rainy = 1 - freq["Rainy", "No"]^2 - freq["Rainy", "Yes"]^2
GINI_Outlook = freqSum["Sunny"] * GINI_Sunny + freqSum["Rainy"] * GINI_Rainy
  # Temperature
absfreq = table(weather[, c(2, 4)])
freq = prop.table(absfreq, 1)
freqSum = rowSums(prop.table(absfreq))
GINI_Hot = 1 - freq["Hot", "No"]^2 - freq["Hot", "Yes"]^2
GINI_Cool = 1 - freq["Cool", "No"]^2 - freq["Cool", "Yes"]^2
GINI_Temperature = freqSum["Hot"] * GINI_Hot + freqSum["Cool"] * GINI_Cool
  # Humidity
absfreq = table(weather[, c(3, 4)])
freq = prop.table(absfreq, 1)
freqSum = rowSums(prop.table(absfreq))
GINI_High = 1 - freq["High", "No"]^2 - freq["High", "Yes"]^2
GINI_Low = 1 - freq["Low", "No"]^2 - freq["Low", "Yes"]^2
GINI_Humidity = freqSum["High"] * GINI_High + freqSum["Low"] * GINI_Low

#or use my function Gini
GinisOutlook = Gini(weather,1,4)
GinisTemperature = Gini(weather,2,4)
GinisHumidity = Gini(weather,3,4)

bestGini = min(GINI_Outlook,GINI_Temperature,GINI_Humidity)

# INFORMATION GAIN
freq2 = prop.table(table(weather[, c(4)]))
Entropy_All = - freq2["No"] * log2(freq2["No"]) - freq2["Yes"] * log2(freq2["Yes"])
  # Outlook
absfreq = table(weather[, c(1, 4)])
freq = prop.table(absfreq, 1)
freqSum = rowSums(prop.table(absfreq))
Entropy_Sunny = - freq["Sunny", "No"] * log2(freq["Sunny", "No"]) - freq["Sunny", "Yes"] * log2(freq["Sunny", "Yes"])
Entropy_Rainy = - freq["Rainy", "No"] * log2(freq["Rainy", "No"]) - freq["Rainy", "Yes"] * log2(freq["Rainy", "Yes"])
GAIN_Outlook = Entropy_All - freqSum["Sunny"] * Entropy_Sunny - freqSum["Rainy"] * Entropy_Rainy
  # Temperature
absfreq = table(weather[, c(2, 4)])
freq = prop.table(absfreq, 1)
freqSum = rowSums(prop.table(absfreq))
Entropy_Hot = - freq["Hot", "No"] * log2(freq["Hot", "No"]) - freq["Hot", "Yes"] * log2(freq["Hot", "Yes"])
Entropy_Cool = - freq["Cool", "No"] * log2(freq["Cool", "No"]) - freq["Cool", "Yes"] * log2(freq["Cool", "Yes"])
GAIN_Temperature = Entropy_All - freqSum["Hot"] * Entropy_Hot - freqSum["Cool"] * Entropy_Cool
  # Humidity
absfreq = table(weather[, c(3, 4)])
freq = prop.table(absfreq, 1)
freqSum = rowSums(prop.table(absfreq))
Entropy_High = - freq["High", "No"] * log2(freq["High", "No"]) - freq["High", "Yes"] * log2(freq["High", "Yes"])
Entropy_Low = - freq["Low", "No"] * log2(freq["Low", "No"]) - freq["Low", "Yes"] * log2(freq["Low", "Yes"])
GAIN_Humidity = Entropy_All - freqSum["High"] * Entropy_High - freqSum["Low"] * Entropy_Low

#or use my functions EntropyAll and InfoGain
Entropy = EntropyAll(weather,4)
GainOutlook = InfoGain(weather,1,4) # first value of Partial Entropy corresponds to sunny
GainTemperature = Gini(weather,2,4)
GainHumidity = Gini(weather,3,4)

bestGain = max(GAIN_Outlook,GAIN_Humidity,GAIN_Temperature)

################################################################################

# 3. Construction and Implementation of a Decision Tree

# import dataset
iris2 = iris[, c(1, 2, 5)]
iris2$Species[c(101:150)] = iris2$Species[c(21:70)]
iris2$Species = factor(iris2$Species)
# split dataset into training and testing data
trainingdata = iris2[c(1:40, 51:90, 101:140),]
testdata = iris2[c(41:50, 91:100, 141:150),]

# Construct the decision tree using the training data 
model1 <- rpart(Species ~ ., method = "class", data = trainingdata, minsplit = 20)
rpart.plot(model1, extra = 104, nn = TRUE)
model2 <- rpart(Species ~ ., method = "class", data = trainingdata, minsplit = 10)
rpart.plot(model2, extra = 104, nn = TRUE)
model3 <- rpart(Species ~ ., method = "class", data = trainingdata, minsplit = 30)
rpart.plot(model3, extra = 104, nn = TRUE)

# Apply the model to the testing data
xtest = testdata[,1:2]
ytest = testdata[,3]
ypred1 = predict(model1, xtest, type="class")
ypred2 = predict(model2, xtest, type="class")
ypred3 = predict(model3, xtest, type="class")

# Calculate metrics
cm1 = as.matrix(table(Actual = ytest, Predicted = ypred1))
accuracy1 = sum(diag(cm1)) / sum(cm1)
precision1 = diag(cm1) / colSums(cm1)
recall1 = diag(cm1) / rowSums(cm1)
f11 = 2 * precision1 * recall1 / (precision1 + recall1)

cm2 = as.matrix(table(Actual = ytest, Predicted = ypred2))
accuracy2 = sum(diag(cm2)) / sum(cm2)
precision2 = diag(cm2) / colSums(cm2)
recall2 = diag(cm2) / rowSums(cm2)
f12 = 2 * precision2 * recall2 / (precision2 + recall2)

cm3 = as.matrix(table(Actual = ytest, Predicted = ypred3))
accuracy3 = sum(diag(cm3)) / sum(cm3)
precision3 = diag(cm3) / colSums(cm3)
recall3 = diag(cm3) / rowSums(cm3)
f13 = 2 * precision3 * recall3 / (precision3 + recall3)
  







