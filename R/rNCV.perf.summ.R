#' Summarize model performance in rNCV

rNCV.perf.summ <- function(rNCV.obj){
  perf <- data.frame(rNCV.obj$perf.train[, -1])
  perf$dataset <- 'train'
  tmp <- data.frame(rNCV.obj$perf.test)
  metrics <- names(tmp); metrics <- metrics[-length(metrics)]
  tmp$dataset <- 'test'
  perf <- rbind(perf[, c('dataset', 'method', metrics)],
                tmp[, c('dataset', 'method', metrics)])
  perf <- reshape(perf,
                  varying=metrics,
                  v.name='performance',
                  timevar = 'metric',
                  times = metrics,
                  direction='long')
  if (!'doBy' %in% installed.packages()){ install.packages('doBy') }
  library(doBy)
  summ <- summaryBy(performance ~ dataset + metric + method, data=perf, FUN=c(mean, sd))
  names(summ)[4:5] <- c('m', 'se')
  summ$method <- as.factor(summ$method)
  summ$method <- relevel(summ$method, ref='Stack')
  return(summ)
}
