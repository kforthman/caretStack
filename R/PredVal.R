#' Predictive Values of Each Base Learner in Each Data Set
#'
#' To assess model performance in the training and testing sets, we need:
#' \enumerate{
#' \item Predictived values of each base learner in each data set.
#' \item If a stack model is build on the top of individual base learners, then we also need the predictive values of the stack model in both sets.
#' \item Compute performance metrics.
#' }
#'
#' For consistency purpose (with stacking predictions), I use \code{defaultSummary(pred)}
#'
#' In PredVal, you can
#' \enumerate{
#' \item specify a stacking method
#' \item specify a weight for each ML algorithm
#' }
#'
#' @param ref.lv reference level for categorical variables.
#' @param stack.wt ???
#' @inheritParams caret::train
#'



PredVal <- function(models, TestSet, resp.var, ref.lv=NULL, method='none',
                    metric=NULL, stack.wt=NULL, trControl=NULL, tuneLength=NULL){
  res <- list(train = NULL, test = NULL)

  if (method=='wt.avg'){
    if (is.null(stack.wt)){
      stack.wt <- sapply(models, function(x) getTrainPerf(x)[, paste0('Train', metric)])
      if (metric %in% c('MAE', 'RMSE', 'logLoss')){
        stack.wt <- 1/stack.wt
      }
      stack.wt <- stack.wt/sum(stack.wt)
    }
    if (class(stack.wt)!='matrix'){
      stack.wt <- matrix(stack.wt/sum(stack.wt), ncol=1)
    }
    rownames(stack.wt) <- names(models)
  }

  if (models[[1]]$modelType=='Regression'){
    for (m in 1:length(models)){
      res$train[[m]] <- models[[m]]$pred[, c('rowIndex', 'Resample', 'obs', 'pred')]
      res$test[[m]]  <- data.frame(
        obs=TestSet[, resp.var],
        pred=as.numeric(predict(models[[m]], newdata=TestSet[, names(TestSet)!=resp.var])) )
    }

    if (method!='none'){
      res$train[[length(models)+1]] <- models[[1]]$pred[, c('rowIndex', 'Resample', 'obs', 'pred')]
      res$test[[length(models)+1]] <- data.frame(obs = TestSet[, resp.var])
      if (method=='wt.avg'){
        tmp <- unlist(lapply(models, function(x) x['pred']), F)
        res$train[[length(models)+1]]$pred <-
          Reduce('+', Map('*', lapply(tmp, function(x) x[, 'pred']) , stack.wt))

        tmp <- lapply(res$test[1:length(models)], function(x) x[,'pred'])
        res$test[[length(models)+1]] <-
          data.frame(obs  = TestSet[, resp.var],
                     pred = Reduce('+', Map('*', tmp, stack.wt)))
      }
      else {
        mtx.stack.train <- sapply(models, function(x) x$pred$pred)
        mtx.stack.test  <- sapply(res$test[1:length(models)], function(x) x$pred)
        colnames(mtx.stack.test) <- colnames(mtx.stack.train)
        if (method=='rf'){
          stack.model <- train(mtx.stack.train, models[[1]]$pred$obs,
                               method = method,
                               trControl = trControl,
                               metric=metric,
                               importance = T,
                               tuneLength = tuneLength)
        }
        else {
          stack.model <- train(mtx.stack.train, models[[1]]$pred$obs,
                               method = method,
                               trControl = trControl,
                               metric=metric,
                               tuneLength = tuneLength)
        }
        stack.wt <- as.matrix(varImp(stack.model)$importance, ncol=1)
        stack.wt <- stack.wt/sum(stack.wt)
        res$train[[length(models)+1]]$pred <- predict(stack.model, newdata=mtx.stack.train)
        res$test[[length(models)+1]]$pred  <- predict(stack.model, newdata=mtx.stack.test)
      }
    }
  }

  else if (models[[1]]$modelType=='Classification'){
    resp.lv <- levels(models[[1]]$pred$obs)
    for (m in 1:length(models)){
      res$train[[m]] <- models[[m]]$pred[, c('rowIndex', 'Resample', 'obs', 'pred', resp.lv)]
      res$test[[m]]  <- data.frame(
        obs = TestSet[, resp.var],
        predict(models[[m]], newdata=TestSet[, names(TestSet)!=resp.var], type='prob')
      )
    }

    if (method!='none'){
      res$train[[length(models)+1]] <-
        models[[1]]$pred[, c('rowIndex', 'Resample', 'obs', 'pred', resp.lv)]
      res$test[[length(models)+1]] <- data.frame(obs = TestSet[, resp.var])
      if (method=='wt.avg'){
        tmp <- unlist(lapply(models, function(x) x['pred']), F)
        res$train[[length(models)+1]][, resp.lv] <-
          Reduce('+', Map('*', lapply(tmp, function(x) x[, resp.lv]), stack.wt))

        tmp <- lapply(res$test[1:length(models)], function(x) x[, resp.lv])
        res$test[[length(models)+1]][, resp.lv] <- Reduce('+', Map('*', tmp, stack.wt))
      }
      else {
        lv.for.stack <- resp.lv[resp.lv != ref.lv]
        if (length(lv.for.stack)==1){
          mtx.stack.train <- sapply(res$train[1:length(models)], function(x) x[, lv.for.stack])
          mtx.stack.test  <- sapply(res$test[1:length(models)], function(x) x[, lv.for.stack])
        }
        else {
          tmp <- sapply(res$train[1:length(models)], function(x) x[, lv.for.stack])
          mtx.stack.train <- Reduce(function(...) cbind(...), tmp)
          tmp <- sapply(res$test[1:length(models)], function(x) x[, lv.for.stack])
          mtx.stack.test <- Reduce(function(...) cbind(...), tmp)
        }
        colnames(mtx.stack.test) <- colnames(mtx.stack.train) <-
          as.vector(sapply(names(models), paste0, paste0('.', lv.for.stack)))

        if (method=='rf'){
          stack.model <- train(mtx.stack.train, res$train[[1]]$obs,
                               method = method,
                               trControl = trControl,
                               metric = metric,
                               importance = T,
                               tuneLength = tuneLength)
        }
        else {
          stack.model <- train(mtx.stack.train, res$train[[1]]$obs,
                               method = method,
                               trControl = trControl,
                               metric = metric,
                               tuneLength = tuneLength)
        }
        stack.wt <- as.matrix(varImp(stack.model)$importance, ncol=1)
        stack.wt <- stack.wt/sum(stack.wt)
        res$train[[length(models)+1]][, resp.lv] <-
          predict(stack.model, newdata=mtx.stack.train, type='prob')[, resp.lv]
        res$test[[length(models)+1]][, resp.lv] <-
          predict(stack.model, newdata=mtx.stack.test, type='prob')[, resp.lv]
      }

      # predicted classes based on predicted probabilities
      res$train[[length(models)+1]]$pred <- factor(
        resp.lv[apply(res$train[[length(models)+1]][,resp.lv], 1, which.max)]
      )
    }
    for (m in 1:(length(res$test))){
      res$test[[m]]$pred <- factor(resp.lv[apply(res$test[[m]][,resp.lv], 1, which.max)])
      if(length(levels(res$test[[m]]$pred)) < length(resp.lv)){
        warning(paste0("Levels of predicted and observed data do not match. Levels of predicted data: ", paste(levels(res$test[[m]]$pred), collapse = ", "), ". Levels of observed data: ", paste(resp.lv, collapse = ", "), ". Forcing levels to match."))
      }
      levels(res$test[[m]]$pred) <- resp.lv
    }
  }

  if (method=='none'){
    names(res$train) <- names(res$test) <- names(models)
  }
  else {
    names(res$train) <- names(res$test) <- c(names(models), 'Stack')
  }

  out <- list(prediction=res, weight=stack.wt)
  if (method=='wt.avg') { out$stack.model <- 'weighted average' }
  else if (!method %in% c('none','wt.avg')) { out$stack.model <- stack.model }
  return(out)
}
