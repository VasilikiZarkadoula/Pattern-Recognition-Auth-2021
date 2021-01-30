library(gsubfn)
Gini <- function(data, X, Y) {
  
  absfreq = table(data[, c(X, Y)])
  freq = prop.table(absfreq, 1)
  freqSum = rowSums(prop.table(absfreq))
  
  uniqueYValues = unique(data[,Y])
  uniqueXVlaues = unique(data[,X])
  
  GiniArray = c()
  for (a in 1:length(uniqueXVlaues)){
    GiniArray = c(GiniArray, 1)
  }
  
  for (i in 1:length(uniqueXVlaues)){
    for (j in 1:length(uniqueYValues)){
      GiniArray[i] = GiniArray[i] - freq[uniqueXVlaues[i],uniqueYValues[j]]^2
    }
  }
  
  
  GiniAll = 0
  for (i in 1:length(uniqueXVlaues)){
    GiniAll = GiniAll + freqSum[uniqueXVlaues[i]]*GiniArray[i]
  }
  
  return(list("Indices" = uniqueXVlaues,"PartialGini" = GiniArray, "Gini" = GiniAll ))
}
  