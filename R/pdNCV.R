# partial-depedence plots using repeated nested CV objects
# current PDP only allows stacking by weighted average #
## In classification, stacking by other learner uses different "predictor names"
pdNCV <- function(sel.var, resp.var, nRep, nFolds, dir.path, file.root, rncv.obj, stack.wt){
  for (j in 1:length(sel.var)) {
    for (r in 1:nRep){
      # outer loop
      for (f in 1:nFolds){
        load(paste0(dir.path, resp.var, '_', file.root, '_Rep_', r, '_fold_', f, '.rda'))
        if (is.null(stack.wt)){
          stack.wt <- rncv.obj$stack.wt
          stack.wt <- t(as.matrix(stack.wt[stack.wt$Rep==r & stack.wt$fold==f, -c(1:2)]))
        }
        final <- ifelse(length(models)==1, names(models), 'Stack')
        pd.est <- pdEst(models, final, sel.var[j], stack.model=NULL, stack.wt)
        if (r==1 & f==1){
          plot(pd.est[,sel.var[j]], pd.est[,final], xlab=' ', ylab='yhat', type='l', main=sel.var[j])
        } else {
          lines(pd.est[,sel.var[j]], pd.est[,final])
        }
      }
    }
  }
}
