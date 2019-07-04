#' No Description.

# NOTE: my notation of "train.by.fold" refers to the "testing" folds
# i.e. a full dataset is divided into k parts for "training" and "testing" (outer-loop)
#      each training set is further divided into k parts for parameter optimization (inner loop)

permuPred <- function(TrainSet, TestSet, resp.var, ref.lv=NULL,
                      control, preProc.opt, tuneL, metric, methods,
                      stack.method='none', weighted.by=NULL, stack.wt=NULL,
                      control.stack=NULL, select='none', nPerm){
  control$allowParallel <- F
  if (!is.null(control.stack$allowParallel)){
    control.stack$allowParallel <- F
  }

  res <- foreach(r=1:nPerm, .combine=rbind, .packages='caret') %dopar% {

    # Step 0. Shuffle the response variable in the TrainSet
    TrainSet[, resp.var]  <- sample(TrainSet[, resp.var], nrow(TrainSet), replace=F)
    TestSet[, resp.var]  <- sample( TestSet[, resp.var], nrow(TestSet) , replace=F)

    # Step 1. build multiple base learners
    models <- caretModels(TrainSet, resp.var, control, preProc.opt, tuneL, metric, methods)

    # Step 2. extract predicted values
    pred.val <- PredVal(models, TestSet, resp.var, ref.lv, stack.method,
                        weighted.by, stack.wt, control.stack, tuneL)

    # Step 3. compute performance metrics
    perf <- modelPerf.summ(pred.val$prediction)
    perf.train <- perf$train.by.fold
    perf.test  <- perf$test
    if (length(methods)>1) {
      if (select=='stack'){
        perf.train <- perf.train[perf.train$method=='Stack',]
        perf.test  <- perf.test[rownames(perf.test)=='Stack',]
      }
      else {
        tmp <- perf.train[perf.train$metric==metric,]
        if (select=='oneSE'){
          if (metric %in% c('RMSE', 'MAE')){
            up <- (tmp$m + tmp$se)[which.min(tmp$m)]
            final.model <- tmp$method[tmp$m==max(tmp$m[tmp$m < up])]
          }
          else {
            lo <- (tmp$m - tmp$se)[which.max(tmp$m)]
            final.model <- tmp$method[tmp$m==min(tmp$m[tmp$m > lo])]
          }
        }
        else if (select=='maxmin'){
          if (metric %in% c('RMSE', 'MAE', 'logLoss')){
            ind <- which.min(tmp$m)
          }
          else { ind <- which.max(tmp$m[tmp$metric==metric.i]) }
          final.model <- tmp$method[ind]
        }
        perf.train <- perf.train[perf.train$method==final.model,]
        perf.test <- perf.test[rownames(perf.test)==final.model,]
      }
    }

    perf.train <- reshape(perf.train[,1:3], timevar='metric', idvar='method', direction='wide')
    perf.test[is.nan.data.frame(perf.test)] <- 0

    if (class(perf.test)=='matrix'){ tmp.names = colnames(perf.test) }
    else if (class(perf.test)=='numeric'){ tmp.names = names(perf.test) }
    perf.test <- data.frame(matrix(perf.test, nrow=1))
    names(perf.test) <- tmp.names
    perf <- data.frame(perf.train, perf.test)
    return(perf)
  }

  return(res)
}
