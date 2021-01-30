OverallGini <- function(data, Y){
  
  # calculate frequencies
  absfreq = table(data[,Y])
  freqSum = prop.table(absfreq)
  
  uniqueValues = unique(data[,Y])
  # calculate GINI for *Insurance*
  Gini = 1
  for(i in 1:length(uniqueValues)){
    Gini = Gini - freqSum[uniqueValues[i]]^2
  }
  
  # print *Insurance* GINI
  return(Gini)
  
}