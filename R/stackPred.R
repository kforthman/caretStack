#' Predict a new set of data using a caretStack model.
#'
#' Supports classification and regression.
#' Note: only continuous variables are expected to be used as predictors. It is assumed that there are a sufficient number of subjects in each category.
#'
#' @param data The data frame containing the training set.
#' @param resp.var Variable to predict.
#' @param nRep Number of times nCV is repeated.
#' @param nFolds.outer Number of outer folds
#' @param dir.path Directory where the CV data is stored. Example: "Output/rNCV/"
#' @param file.root Prefix for the CV filenames.
#' @param stack.method
#' @export
stackPred <- function(data, resp.var, nRep, nFolds.outer,
                      dir.path, file.root,
                      stack.method='wt.avg'){
  if(stack.method == 'wt.avg'){
    # rep.pred will contain the averages over the folds
    rep.pred <- list()
    for(r in 1:nRep){
      # fold.pred will contain the weighted predictions within each fold
      fold.pred <- list()
      for(k.outer in 1:nFolds.outer) {

        # Load in the model and PredVal files.
        load(paste0(dir.path, resp.var, '_', file.root, '_Rep_', r, '_fold_', k.outer, '.rda'))
        load(paste0(dir.path, resp.var, '_', file.root, '_Rep_', r, '_fold_', k.outer, '-PredVal.rda'))

        if (models[[1]]$modelType=='Regression'){
          # total.pred will hold the summed weighted predictions across models.
          total.pred <- matrix(0, ncol = 1, nrow = nrow(data))
          for (m in 1:length(models)){
            total.pred <- total.pred + (pred.val$weight[[m]] * predict(models[[m]], data))
            # Result will have levels in the same order as models.
          }
        }else if (models[[1]]$modelType=='Classification'){
          # total.pred will hold the summed weighted predictions across models.
          total.pred <- matrix(0, ncol = length(models[[1]]$levels), nrow = nrow(data))
          for (m in 1:length(models)){
            # total.pred will hold the summed weighted predictions across models.
            # type is set to "prob" to get a likelihood.
            total.pred <- total.pred + (pred.val$weight[[m]] * predict(models[[m]], data, type = "prob"))
            # Result will have levels in the same order as models.
          }
        }
        fold.pred[[k.outer]] <- total.pred
      }
      total.fold <- matrix(0, ncol = ncol(fold.pred[[1]]), nrow = nrow(fold.pred[[1]]))
      for(kk in 1:nFolds.outer){total.fold <- total.fold + fold.pred[[kk]]}
      fold.pred.avg <- total.fold/nFolds.outer
      rep.pred[[r]] <- fold.pred.avg
    }
    total.rep <- matrix(0, ncol = ncol(rep.pred[[1]]), nrow = nrow(rep.pred[[1]]))
    for(rr in 1:nRep){total.rep <- total.rep + rep.pred[[rr]]}
    rep.pred.avg <- total.rep/nRep

    return(rep.pred.avg)
  }else{stop(paste0("The stack method '", stack.method, "' is not supported yet. Please use 'wt.avg' or submit an issue."))}
}
