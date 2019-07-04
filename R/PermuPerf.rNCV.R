PermuPerf.rNCV <- function(data, resp.var, ref.lv=NULL, nRep, nFolds.outer, ML.methods,
                           control, tuneL, preProc.opt, metric, dir.path, file.root,
                           stack.method='wt.avg', weighted.by=NULL, stack.wt=NULL,
                           control.stack=NULL, nPerm){

  if (class(data[, resp.var])=='factor')
  { resp.lv = levels(data[, resp.var])
  } else if (class(data[, resp.var])=='character')
  { resp.lv = unique(data[, resp.var])
  } else { resp.lv = 'pred' }
  control$allowParallel <- F
  if (!is.null(control.stack$allowParallel)){
    control.stack$allowParallel <- F
  }

  res <- foreach(perm=1:nPerm, .combine=rbind, .packages=c('caret')) %dopar% {
    data[, resp.var] <- sample(data[, resp.var], nrow(data), replace=F)
    suppressWarnings(
      perf.perm <- rNCV.perm(data, resp.var, ref.lv, nRep, nFolds.outer, ML.methods,
                             control, tuneL, preProc.opt, metric, dir.path, file.root,
                             stack.method, weighted.by, stack.wt, control.stack)
    )
    return(perf.perm)
  }
  return(res)
}
