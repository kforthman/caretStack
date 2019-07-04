#' No Description.

# NOTE: my notation of "train.by.fold" refers to the "testing" folds
# i.e. a full dataset is divided into k parts for "training" and "testing" (outer-loop)
#      each training set is further divided into k parts for parameter optimization (inner loop)

modelPerf.summ <- function(predval.list){
  library(plyr)
  perf.train <- lapply(predval.list$train, function(x) ddply(x, .(Resample), modelPerf))
  perf.test <- sapply(predval.list$test , modelPerf)
  m <- data.frame(sapply(perf.train, function(x) apply(x[,-1], 2, mean, na.rm=T)))
  s <- data.frame(sapply(perf.train, function(x) apply(x[,-1], 2, sd, na.rm=T)))
  m$metric <- rownames(m)
  s$metric <- rownames(s)
  m <- reshape(m, varying = names(m)[names(m)!='metric'], v.names = 'm',
               timevar = 'method', times = names(m)[names(m)!='metric'], direction = 'long')
  s <- reshape(s, varying = names(s)[names(s)!='metric'], v.names = 'se',
               timevar = 'method', times = names(s)[names(s)!='metric'], direction = 'long')
  train.by.fold <- merge(m, s, by=c('method', 'metric'))
  train.by.fold <- train.by.fold[c('method', 'metric', 'm', 'se')]
  perf.test <- perf.test[order(rownames(perf.test)),]
  perf.test <- t(perf.test)
  return(list(train.by.fold=train.by.fold, test=perf.test))
}
