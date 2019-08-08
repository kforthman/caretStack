#' Summarize model performance in rNCV
#'
#' Gives a summary of model performance. Returns average of each metric specified in rNCV (whether MAE, RMSE, R^2, or something else) for both the training and testing set for each method.
#' @param rNCV.obj The rNCV object returned by the function \code{rNCV()}.
#' @importFrom doBy summaryBy

rNCV.perf.summ <- function(rNCV.obj){
  # perf is the training set. The last column (R squared) is removed.
  perf <- data.frame(rNCV.obj$perf.train[, -1])
  perf$dataset <- 'train'

  # tmp is the testing set.
  tmp <- data.frame(rNCV.obj$perf.test)

  # pull the names of the different methods used for this particular rNCV.
  metrics <- names(tmp)
  metrics <- metrics[-length(metrics)]

  tmp$dataset <- 'test'

  # merge the testing and training sets.
  perf <- rbind(perf[, c('dataset', 'method', metrics)],
                tmp[, c('dataset', 'method', metrics)])
  perf <- reshape(perf,
                  varying=metrics,
                  v.name='performance',
                  timevar = 'metric',
                  times = metrics,
                  direction='long')

  # average the performance over dataset, method, and metric.
  summ <- summaryBy(performance ~ dataset + metric + method, data=perf, FUN=c(mean, sd))
  names(summ)[4:5] <- c('m', 'se')
  summ$method <- as.factor(summ$method)
  summ$method <- relevel(summ$method, ref='Stack') # This simply sets Stack as the first level.

  return(summ)
}
