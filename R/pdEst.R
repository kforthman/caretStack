#' No Description.

pdEst <- function(models, final, pred.var, stack.model=NULL, stack.weight=NULL){
  if (final!='Stack'){
    pd.est <- partial(models[[names(models)==final]], pred.var, plot=F, rug=TRUE)
  }
  else {
    # pd estimates for the first method
    pd.est <- partial(models[[1]], pred.var, plot=F, rug=T)
    names(pd.est)[2] <- names(models)[1]
    # pd for the other methods
    for (m in 2:length(models)){
      tmp <- partial(models[[m]], pred.var, plot=F, rug=T)
      names(tmp)[2] <- names(models)[m]
      pd.est <- merge(pd.est, tmp, by=pred.var)
    }
    pd.est$Stack <- NA
    y.hat <- data.frame(matrix(NA, nrow(models[[1]]$trainingData), length(models)))
    names(y.hat) <- names(models)
    for (i in 1:nrow(pd.est)){
      dat.tmp <- models[[1]]$trainingData
      dat.tmp <- dat.tmp[, names(dat.tmp)!='.outcome']
      # replace the predictor column by the same (i-th unique) value
      dat.tmp[, pred.var] <- pd.est[i, pred.var]
      # compute predictive values for each base learner
      for (m in 1:length(models)){
        if (models[[1]]$modelType=='Classification'){
          tmp <- predict(models[[m]], newdata=as.matrix(dat.tmp), type='prob')
        }
        else { tmp <- predict(models[[m]], newdata=as.matrix(dat.tmp)) }
        if (class(tmp)=='matrix'){ tmp <- tmp[, 'y']}
        y.hat[,m] <- tmp
      }
      # predicted values of the stack model
      if (!is.null(stack.model)){
        yhat.stack <- predict(stack.model, newdata=as.matrix(y.hat))
      }
      else if (!is.null(stack.weight)){
        yhat.stack <- as.matrix(y.hat) %*% as.matrix(stack.weight, ncol=1)
      }
      pd.est$Stack[i] <- mean(yhat.stack) # partial dependece is the mean of all predicted values
    }
  }
  return(pd.est)
}
