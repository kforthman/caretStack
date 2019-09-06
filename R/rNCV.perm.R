#' rNCV.perm is identical to rNCV except do instead of dopar
#'
#' 2018-08-07 version: use ensemble predictions from rNCV for permutation ###
#'
#' Parallel at k-fold CV (previous version in Replicates) ##
#'
#' rNCV.perm is identical to rNCV except
#' (1) do instead of dopar (parallel for permutation replicates)
#' (2) res contains only predictive values/probabilities (i.e. y.pred.comb)
#' (3) use ensemble predictions for model performance in rNCV() and rNCV.perm()

rNCV.perm <- function(data, resp.var, ref.lv=NULL, nRep, nFolds.outer, ML.methods,
                      control, tuneL, preProc.opt, metric, dir.path, file.root,
                      stack.method='wt.avg', weighted.by=NULL, stack.wt=NULL, control.stack=NULL){

  if (class(data[, resp.var])=='factor')
  { resp.lv = levels(data[, resp.var])
  } else if (class(data[, resp.var])=='character')
  { resp.lv = unique(data[, resp.var])
  } else { resp.lv = 'pred' }
  control$allowParallel <- F
  if (!is.null(control.stack$allowParallel)){
    control.stack$allowParallel <- F
  }

  res <- foreach(r=1:nRep, .combine=cbind, .packages='caret') %do% {
    index.outer <- createFolds(data[, resp.var], k=nFolds.outer, list=F)

    weight <- perf.by.fold <- var.imp <- NULL; stack.model <- list()
    y.pred.comb <- matrix(NA, nrow(data), length(resp.lv))
    colnames(y.pred.comb) <- resp.lv

    for(k.outer in 1:nFolds.outer) {
      calib <- data[index.outer!=k.outer, ]
      test.set <- data[index.outer==k.outer, ]

      # Step 1. Build base learners
      models <- caretModels(calib, resp.var, control, preProc.opt, tuneL, metric, ML.methods)
      if (!is.null(dir.path) & !is.null(file.root)){
        save(models,
             file = paste0(dir.path, resp.var, '_', file.root, '_Rep_', r, '_fold_', k.outer, '.rda'))
      }

      # Step 2. Extract predicted values/probabilities
      pred.val <- PredVal(models, test.set, resp.var, ref.lv, stack.method,
                          weighted.by, stack.wt, control.stack, tuneL)

      if (length(ML.methods)>1 & !stack.method %in% c('none'))
      {
        stack.model[[k.outer]] <- pred.val$stack.model
        weight <- rbind(weight, data.frame(Rep = r, fold = k.outer, t(pred.val$weight)))
      }
      ## predicted values/probabilities across folds ##
      if (length(ML.methods)==1)
      { y.pred.comb[index.outer==k.outer, ] <- as.matrix(pred.val$prediction$test[[1]][, resp.lv]) }
      else
      { y.pred.comb[index.outer==k.outer, ] <- as.matrix(pred.val$prediction$test$Stack[, resp.lv]) }

    }

    return(y.pred.comb = y.pred.comb)
  }

  if ('pred' %in% resp.lv)
  { colnames(res) <- paste0('pred.Rep', 1:nRep)
  df.ensemble <- data.frame(obs = data[, resp.var], pred = rowMeans(res))
  }
  else if (!'pred' %in% resp.lv)
  { suppressWarnings(
    colnames(res) <- levels(interaction(resp.lv, paste0('Rep', 1:nRep)))
  )
    df.ensemble <- setNames(data.frame(matrix(NA, nrow(res), length(resp.lv))), resp.lv)
    for (j in resp.lv){
      df.ensemble[, j] <- rowMeans(res[, grep(j, colnames(res))])
    }
    df.ensemble$pred <- factor(resp.lv[apply(df.ensemble, 1, which.max)])
    df.ensemble$obs <- data[, resp.var]
  }

  return(modelPerf(df.ensemble))
}
