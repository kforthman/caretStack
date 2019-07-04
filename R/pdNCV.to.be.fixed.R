#' No Description.

pdNCV.to.be.fixed <- function(sel.var, resp.var, nRep, nFolds, dir.path, file.root, rncv.obj, stack.method, stack.wt){
  for (j in 1:length(sel.var)) {
    for (r in 1:nRep){
      for (f in 1:nFolds){
        load(paste0(dir.path, resp.var, '_', file.root, '_Rep_', r, '_Fold_', f, '.rda'))
        if (stack.method=='none' | length(models)==1){
          final <- names(models)
          pd.est <- pdEst(models, final, sel.var[j], stack.model=NULL, stack.weight=NULL)
        } else {
          final <- 'Stack'
          if (stack.method=='wt.avg'){
            if (is.null(stack.wt)){
              stack.wt <- rncv.obj$stack.wt
              stack.wt <- t(as.matrix(stack.wt[stack.wt$Rep==r & stack.wt$fold==f, -c(1:2)]))
            }
            pd.est <- pdEst(models, final, sel.var[j], stack.model=NULL, stack.wt)
          }
          else {
            load(paste0(dir.path, resp.var, '_', file.root, '_stack.model_Rep_', r, '.rda'))
            pd.est <- pdEst(stack.model[[f]], final, sel.var[j],
                            stack.model=stack.model[[f]], stack.weight=NULL)
          }
        }
        if (r==1 & f==1){
          plot(pd.est[,sel.var[j]], pd.est[,final], xlab=' ', ylab='yhat', type='l', main=sel.var[j])
        } else {
          lines(pd.est[,sel.var[j]], pd.est[,final])
        }
      }
    }
  }
}
