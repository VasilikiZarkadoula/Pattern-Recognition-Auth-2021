source("EntropyAll.R")
InfoGain <- function(data, X, Y) {
  absfreq = table(data[,c(X,Y)])
  freq = prop.table(absfreq, 1)
  freqSum = rowSums(prop.table(absfreq))
  uniqueXValues = unique(data[,X])
  uniqueYValues = unique(data[,Y])
  n = length(uniqueXValues)
  m = length(uniqueYValues)
  
  Entropy = EntropyAll(data,Y)
  
  PartialEntropy = rep(0,n);
 
  for( i in 1:n){
    for( j in 1:m){
      X = uniqueXValues[i]
      Y = uniqueYValues[j]
      p = freq[X,Y]
      PartialEntropy[i] = PartialEntropy[i] - p*log2( p )
    }
  }
  
  Gain = Entropy
  for( i in 1:n){
    Gain = Gain - freqSum[uniqueXValues[i]]*PartialEntropy[i]
  }
  
  return(list("Indices" = uniqueXValues,"PartialEntropy" = PartialEntropy, "Gain" = Gain ))
}
