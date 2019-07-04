# a function to extract predicted values (and construct stack models)
PredVal <- function(models, TestSet, resp.var, ref.lv=NULL, stack.method='none',
                    weighted.by=NULL, stack.wt=NULL, control.stack=NULL, tuneL=NULL){
  res <- list(train = NULL, test = NULL)

  if (stack.method=='wt.avg'){
    if (is.null(stack.wt)){
      stack.wt <- sapply(models, function(x) getTrainPerf(x)[, paste0('Train', weighted.by)])
      if (weighted.by %in% c('MAE', 'RMSE', 'logLoss')){
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

    if (stack.method!='none'){
      res$train[[length(models)+1]] <- models[[1]]$pred[, c('rowIndex', 'Resample', 'obs', 'pred')]
      res$test[[length(models)+1]] <- data.frame(obs = TestSet[, resp.var])
      if (stack.method=='wt.avg'){
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
        if (stack.method=='rf'){
          stack.model <- train(mtx.stack.train, models[[1]]$pred$obs,
                               method = stack.method,
                               trControl = control.stack,
                               metric=weighted.by,
                               importance = T,
                               tuneLength = tuneL)
        }
        else {
          stack.model <- train(mtx.stack.train, models[[1]]$pred$obs,
                               method = stack.method,
                               trControl = control.stack,
                               metric=weighted.by,
                               tuneLength = tuneL)
        }
        stack.wt <- as.matrix(varImp(stack.model)$importance, ncol=1)
        stack.wt <- stack.wt/sum(stack.wt)
        res$train[[length(models)+1]]$pred <- predict(stack.model, newdata=mtx.stack.train)
        res$test[[length(models)+1]]$pred  <- predict(stack.model, newdata=mtx.stack.test)
      }
    }
  }

  else if (models[[1]]$modelType=='Classification'){
    resp.lv <- as.character(unique(models[[1]]$pred$obs))
    for (m in 1:length(models)){
      res$train[[m]] <- models[[m]]$pred[, c('rowIndex', 'Resample', 'obs', 'pred', resp.lv)]
      res$test[[m]]  <- data.frame(
        obs = TestSet[, resp.var],
        predict(models[[m]], newdata=TestSet[, names(TestSet)!=resp.var], type='prob')
      )
    }

    if (stack.method!='none'){
      res$train[[length(models)+1]] <-
        models[[1]]$pred[, c('rowIndex', 'Resample', 'obs', 'pred', resp.lv)]
      res$test[[length(models)+1]] <- data.frame(obs = TestSet[, resp.var])
      if (stack.method=='wt.avg'){
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

        if (stack.method=='rf'){
          stack.model <- train(mtx.stack.train, res$train[[1]]$obs,
                               method = stack.method,
                               trControl = control.stack,
                               metric = weighted.by,
                               importance = T,
                               tuneLength = tuneL)
        }
        else {
          stack.model <- train(mtx.stack.train, res$train[[1]]$obs,
                               method = stack.method,
                               trControl = control.stack,
                               metric = weighted.by,
                               tuneLength = tuneL)
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
    }
  }

  if (stack.method=='none'){
    names(res$train) <- names(res$test) <- names(models)
  }
  else {
    names(res$train) <- names(res$test) <- c(names(models), 'Stack')
  }

  out <- list(prediction=res, weight=stack.wt)
  if (stack.method=='wt.avg') { out$stack.model <- 'weighted average' }
  else if (!stack.method %in% c('none','wt.avg')) { out$stack.model <- stack.model }
  return(out)
}
