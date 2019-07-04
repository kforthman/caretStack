#' Build a caret model
#'
#' A function to build prediction models for the training set.
#' @param TrainSet Your training dataset.
#' @param resp.var blah blah
#' @param control
#' @param preProc.opt
#' @param metric
#' @param methods
# a function to build prediction models for the training set
# kernlab requires formula instead of y vector and X matrix
caretModels <- function(TrainSet, resp.var,
                        control, preProc.opt, tuneL, metric, methods){
  L <- length(methods)
  if (is.na(control$repeats)){
    control$repeats <- 1
  }
  control$index <- createMultiFolds(TrainSet[, resp.var],
                                    k = control$number,
                                    times = control$repeats)
  models <- list()
  fm <- as.formula(paste0(resp.var, '~.'))
  for (i in 1:L){
    if (methods[i] == 'rf'){
      models[[i]] <- train(fm, data=TrainSet,
                           method = methods[i],
                           trControl = control,
                           metric=metric,
                           preProcess = preProc.opt,
                           importance = T,
                           tuneLength = tuneL)
    } else if (methods[i] == 'ranger') {
      models[[i]] <- train(fm, data=TrainSet,
                           method = methods[i],
                           trControl = control,
                           metric=metric,
                           preProcess = preProc.opt,
                           importance = 'permutation',
                           tuneLength = tuneL)
    } else {
      models[[i]] <- train(fm, data=TrainSet,
                           method = methods[i],
                           trControl = control,
                           metric=metric,
                           preProcess = preProc.opt,
                           tuneLength = tuneL)
    }
    models[[i]]$pred <- models[[i]]$pred[order(models[[i]]$pred$rowIndex), ]
  }
  names(models) <- methods

  return(models)
}
