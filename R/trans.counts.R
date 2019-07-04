### functions for one training set (TrainSet) and one test set (TestSet) ###

trans.counts <- function(data){
  x.trans <- data
  for (j in 1:ncol(data)){
    x.trans[,j] <- ifelse(data[,j]>0, log(data[,j]), log(0.1))
  }
  return(x.trans)
}
