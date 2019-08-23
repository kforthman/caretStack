#' No Description.
#'
#' Uses results from permTest() for finding significance of model performance. This function may be time-consuming.
#' See lines 589 - 605 for an example.

PermTest.rNCV <- function(rncv.obj, perf.perm, figure){
  metric.names <- names(rncv.obj$perf.ensemble)

  summ <- data.frame(metric   = metric.names,
                     obs = rncv.obj$perf.ensemble,
                     perm.mean = rep(NA, length(metric.names)),
                     perm.se = rep(NA, length(metric.names)),
                     pval     = rep(NA, length(metric.names))      )
  for (i in 1:length(metric.names)){
    o.stat <- summ$obs[summ$metric==metric.names[i]]
    p.stat <- perf.perm[, metric.names[i]]
    summ$perm.mean[i] <- mean(p.stat, na.rm=T)
    summ$perm.se[i] <- sd(p.stat, na.rm=T)
    if (metric.names[i] %in% c('RMSE', 'MAE', 'logLoss')){
      summ$pval[summ$metric==metric.names[i]] <- mean(p.stat < o.stat, na.rm=T)
    } else {
      summ$pval[summ$metric==metric.names[i]] <- mean(p.stat > o.stat, na.rm=T)
    }
    if (figure){
      rg <- range(o.stat, p.stat, na.rm=T)
      plot(density(p.stat), xlim=rg, type='l', col='blue', xlab='', main=metric.names[i])
      abline(v=o.stat, lty=2, col='blue')
    }
  }
  rownames(summ) <- summ$metric
  return(summ[,-1])
}
