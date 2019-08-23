#' Repeated, Nested Cross-Validation
#'
#' Supports classification and regression.
#' Note: only continuous variables are expected to be used as predictors. It is assumed that there are a sufficient number of subjects in each category.
#'
#' @param data The data frame containing the training set.
#' @param nRep Number of times nCV is repeated.
#' @param nFolds.outer Number of outer folds
#' @param dir.path Directory where the CV data is stored.
#' @param file.root Prefix for the CV filenames.
#' @param stack.method ???
#' @param weighted.by ???
#' @param stack.wt ???
#' @param control.stack ???
#' @inheritParams caretModels
#' @inheritParams PredVal

rNCV <- function(data, resp.var, ref.lv=NULL, nRep, nFolds.outer, methods,
                 trControl, tuneLength, preProcess, metric, dir.path, file.root,
                 stack.method='wt.avg', weighted.by=NULL, stack.wt=NULL, control.stack=NULL){
  ptm <- proc.time()

  if (class(data[, resp.var])=='factor')
  { resp.lv = levels(data[, resp.var])
  } else if (class(data[, resp.var])=='character')
  { resp.lv = unique(data[, resp.var])
  } else { resp.lv = 'pred' }
  # trControl$allowParallel <- F
  #if (!is.null(control.stack$allowParallel)){
  #  control.stack$allowParallel <- F
  #}

  #do instead of dopar to make it work
  res <- foreach(r=1:nRep, .combine=comb_rep, .packages='caret') %do% { #%dopar% {
    index.outer <- createFolds(data[, resp.var], k=nFolds.outer, list=F)

    weight <- perf.by.fold <- var.imp <- perf.train <- perf.test <- NULL;
    stack.model <- list()
    y.pred.comb <- matrix(NA, nrow(data), length(resp.lv))
    colnames(y.pred.comb) <- resp.lv

    #  Outer loop
    # partition into nFolds.outer sets
    # (left panel of flow chart)
    for(k.outer in 1:nFolds.outer) {
      calib <- data[index.outer!=k.outer, ] #calibration (training) set
      test.set <- data[index.outer==k.outer, ] #testing set

      # impute missing data
      if (sum(is.na(calib)>0)){
        calib <- knnImputation(calib)
      }
      if (sum(is.na(test.set)>0)){
        test.set <- knnImputation(test.set)
      }

      # Inner loop
      # Step 1. Build base learners
      # (upper right in flow chart)
      # Outputs a file for each resulting fold.
      models <- caretModels(calib, resp.var, trControl, preProcess, tuneLength, metric, methods)
      if (!is.null(dir.path) & !is.null(file.root)){
        save(models,
             file = paste0(dir.path, resp.var, '_', file.root, '_Rep_', r, '_fold_', k.outer, '.rda'))
      }

      # Step 2. Extract predicted values/probabilities
      # Use the models generated in previous step to make prediction on the testing set.
      # Done for each ML algorithm. PredVal combines the predictions from each method.
      pred.val <- PredVal(models, test.set, resp.var, ref.lv, stack.method,
                          weighted.by, stack.wt, control.stack, tuneLength)

      if (length(methods)>1 & !stack.method %in% c('none'))
      { stack.model[[k.outer]] <- pred.val$stack.model
      weight <- rbind(weight, data.frame(Rep = r, fold = k.outer, t(pred.val$weight)))
      }
      ## predicted values/probabilities across folds ##
      if (length(methods)==1){
        y.pred.comb[index.outer==k.outer, ] <- as.matrix(pred.val$prediction$test[[1]][, resp.lv])
      } else {
        y.pred.comb[index.outer==k.outer, ] <- as.matrix(pred.val$prediction$test$Stack[, resp.lv])
      }

      # Step 3. Model performance in the calibrating & hold-out sets of the outer loop
      perf.t.tmp <- lapply(pred.val$prediction$train, function(x) ddply(x, .(Resample), modelPerf))
      perf.train <- do.call(rbind, perf.t.tmp)
      perf.test <- data.frame(modelPerf.summ(pred.val$prediction)$test)
      if (length(methods)==1){
        rownames(perf.test) <- methods
      } else if (length(methods)>1){
        perf.v.tmp <- perf.test[rownames(perf.test)=='Stack', ]
      }
      perf.by.fold <- rbind(perf.by.fold,
                            data.frame(Rep = r, fold = k.outer,
                                       method = rownames(perf.v.tmp), perf.v.tmp))

      # Step 4. Variable importance
      if (length(methods)==1){
        var.imp <- rbind(var.imp, varImp(models[[1]]))
      } else {
        var.imp <- rbind(var.imp,
                         data.frame(
                           Rep = r,
                           fold = k.outer,
                           VarImp(models, 'Stack', weight=pred.val$weight)[, c('variable','Stack')]))
      }
    }
    if (!is.null(dir.path) & !is.null(file.root)){
      save(stack.model, file=
             paste0(dir.path, resp.var, '_', file.root, '_stack.model_Rep_', r, '.rda'))
    }

    if ('pred' %in% resp.lv){
      df.comb <- data.frame(obs = data[, resp.var], y.pred.comb)
    } else if (!'pred' %in% resp.lv){
      df.comb <- data.frame(y.pred.comb)
      df.comb$pred <- factor(resp.lv[apply(df.comb[, resp.lv], 1, which.max)])
      df.comb$obs <- data[, resp.var]
    }
    perf.comb <- modelPerf(df.comb) # trControl$summaryFunction(df.comb)
    perf.train$method <- gsub("\\..*", "", rownames(perf.train) )
    perf.test$method  <- gsub("\\..*", "", rownames(perf.test) )

    return(list(index.outer = index.outer,
                stack.wt     = weight    ,   y.pred.comb = y.pred.comb,
                perf.by.fold = perf.by.fold, perf.comb   = perf.comb,
                perf.train = perf.train,     perf.test = perf.test,
                var.imp     = var.imp))
  }
  if (nRep>1){ colnames(res$index.outer) <- paste0('Rep', 1:nRep) }
  names(res$var.imp)[4] <- 'importance'
  if ('pred' %in% resp.lv){
    colnames(res$y.pred.comb) <- paste0('Rep', 1:nRep)
    df.ensemble <- data.frame(obs = data[, resp.var], pred = rowMeans(res$y.pred.comb))
  } else if (!'pred' %in% resp.lv){
    suppressWarnings(
      colnames(res$y.pred.comb) <- levels(interaction(resp.lv, paste0('Rep', 1:nRep)))
    )
    df.ensemble <- setNames(data.frame(matrix(NA, nrow(res$y.pred.comb), length(resp.lv))), resp.lv)
    for (j in resp.lv){
      df.ensemble[, j] <- rowMeans(res$y.pred.comb[, grep(j, colnames(res$y.pred.comb))])
    }
    df.ensemble$pred <- factor(resp.lv[apply(df.ensemble, 1, which.max)])
    df.ensemble$obs <- data[, resp.var]
  }
  res$perf.ensemble <- modelPerf(df.ensemble)

  res$elapsed.time <- (proc.time() - ptm)[3]
  return(res)
}
