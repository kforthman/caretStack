#' Build a caret model
#'
#' A function to build prediction models for the training set.
#' @param TrainSet The training set.
#' @param resp.var Indicate the name of the column in the training set that contains the response variable.
#' @param methods Similarly to the \code{method} argument in caret's \code{train} function, this argument is a list of strings specifying which classification or regression models to use. Possible values are found using \code{names(getModelInfo())}. See \url{http://topepo.github.io/caret/train-models-by-tag.html}. A list of functions can also be passed for a custom model function. See \url{http://topepo.github.io/caret/using-your-own-model-in-train.html} for details.
#' @inheritParams caret::train
#' @examples
#' ctrl.reg <- trainControl(method = 'cv',               # k-fold cross-validation
#' number = 5,                  # k = 5
#' search = 'grid',             # use grid search over paramter space
#' summaryFunction = defaultSummary,
#' selectionFunction = 'oneSE', # select optimal tuning parameters by "one standard error" rule
#' savePredictions = 'final')   # save predicted values of the final model
#'
#' boston.models <- caretModels(boston.training,    # training set
#'                              resp.var='cmedv',   # response variable
#'                              trControl = ctrl.reg,
#'                              preProcess = c('center', 'scale'),
#'                              tuneLength = 7,
#'                              metric = 'RMSE',
#'                              methods = 'svmRadial')

caretModels <- function(TrainSet, resp.var,
                        trControl, preProcess, tuneLength, metric, methods){
  L <- length(methods)
  if (is.na(trControl$repeats)){
    trControl$repeats <- 1
  }
  trControl$index <- createMultiFolds(TrainSet[, resp.var],
                                    k = trControl$number,
                                    times = trControl$repeats)
  models <- list()
  fm <- as.formula(paste0(resp.var, '~.'))
  for (i in 1:L){
    if (methods[i] == 'rf'){
      models[[i]] <- train(fm, data=TrainSet,
                           method = methods[i],
                           trControl = trControl,
                           metric=metric,
                           preProcess = preProcess,
                           importance = T,
                           tuneLength = tuneLength)
    } else if (methods[i] == 'ranger') {
      models[[i]] <- train(fm, data=TrainSet,
                           method = methods[i],
                           trControl = trControl,
                           metric=metric,
                           preProcess = preProcess,
                           importance = 'permutation',
                           tuneLength = tuneLength)
    } else {
      models[[i]] <- train(fm, data=TrainSet,
                           method = methods[i],
                           trControl = trControl,
                           metric=metric,
                           preProcess = preProcess,
                           tuneLength = tuneLength)
    }
    models[[i]]$pred <- models[[i]]$pred[order(models[[i]]$pred$rowIndex), ]
  }
  names(models) <- methods

  return(models)
}
