#' No Description.

PermTest.rncv.old <- function(perf.obs, perf.perm, figure){
  if ('method' %in% names(perf.obs)){
    perf.obs  <- perf.obs[, -grep('method', names(perf.obs))]
  }
  metric.names <- names(perf.obs)
  summ <- data.frame(metric   = metric.names,
                     obs.mean = apply(perf.obs, 2, mean, na.rm=T),
                     obs.se   = apply(perf.obs, 2, sd  , na.rm=T),
                     pval     = rep(NA, length(metric.names))      )
  for (i in 1:length(metric.names)){
    o.stat <- summ$obs.mean[summ$metric==metric.names[i]]
    p.stat <- perf.perm[, metric.names[i]]
    if (metric.names[i] %in% c('RMSE', 'MAE')){
      summ$pval[summ$metric==metric.names[i]] <- mean(p.stat < o.stat, na.rm=T)
    }
    else {
      if (metric.names[i] %in% c('LR Pos', 'LR Neg')){
        if (o.stat < 1){
          summ$pval[summ$metric==metric.names[i]] <- mean(p.stat < o.stat, na.rm=T)
        } else {
          summ$pval[summ$metric==metric.names[i]] <- mean(p.stat > o.stat, na.rm=T)
        }
      } else {
        summ$pval[summ$metric==metric.names[i]] <- mean(p.stat > o.stat, na.rm=T)
      }
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
