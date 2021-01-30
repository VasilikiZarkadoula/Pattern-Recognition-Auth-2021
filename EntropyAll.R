EntropyAll <- function(data, Y) {
  freqGain = prop.table(table(data[, Y]))
  
  uniqueValues = unique(data[,Y])
  result = 0;
  for(i in uniqueValues){
    p = freqGain[i]
    result = result - p*log2(p)
  }
  return(result)
}
