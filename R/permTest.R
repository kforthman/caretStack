permTest <- function(perf.org.list, final, perf.permu, figure=T){
  if (nrow(perf.org.list$test)==1)
  {
    tmp <- perf.org.list$test
    tmp <- data.frame(set='Testing', metric = colnames(tmp), t(tmp))
  }
  else
  {
    tmp <- perf.org.list$test[rownames(perf.org.list$test)==final,]
    tmp <- data.frame(set='Testing', metric = names(tmp), tmp)
  }

  names(tmp)[3] <- 'm'
  #tmp <- reshape(tmp, varying = names(tmp)[names(tmp)!='method'], v.names = 'm',
  #        timevar = 'metric', times = names(tmp)[names(tmp)!='method'], direction = 'long')
  # tmp <- data.frame(set='Testing', tmp[, c('metric', 'm')])

  perf.pval <- rbind(
    data.frame(set='Training',
               perf.org.list$train.by.fold[perf.org.list$train.by.fold$method==final, c('metric', 'm')]),
    tmp)
  perf.pval <- perf.pval[order(perf.pval$set, perf.pval$metric), ]

  metric.names <- unique(perf.pval$metric)
  perf.permu <- perf.permu[, -grep('method', names(perf.permu))]

  for (i in 1:nrow(perf.pval)){
    if (perf.pval$metric[i] %in% c('MAE', 'RMSE', 'logLoss')){
      perf.pval$p.vale[i] <- mean(perf.permu[, i] < perf.pval$m[i], na.rm=T)
    }
    else {
      perf.pval$p.vale[i] <- mean(perf.permu[, i] > perf.pval$m[i], na.rm=T)
    }
  }

  for (j in metric.names){
    #rg <- range(perf.pval$m[perf.pval$metric==j], perf.permu[, paste0('m.', j)], na.rm=T)
    x.min <- min(perf.org.list$train.by.fold$m[perf.org.list$train.by.fold$metric==j],
                 perf.org.list$test[j], perf.permu[, paste0('m.', j)], perf.permu[, j], na.rm=T)
    x.max <- max(perf.org.list$train.by.fold$m[perf.org.list$train.by.fold$metric==j],
                 perf.org.list$test[j], perf.permu[, paste0('m.', j)], perf.permu[, j], na.rm=T)
    plot(density(perf.permu[, paste0('m.', j)], na.rm=T), xlim=c(x.min, x.max),
         type='l', col='blue', xlab='', main=j)
    abline(v=perf.pval$m[perf.pval$set=='Training' & perf.pval$metric==j], lty=2, col='blue')
    lines(density(perf.permu[,j], na.rm=T), col='red')
    abline(v=perf.pval$m[perf.pval$set=='Testing' & perf.pval$metric==j], lty=2, col='red')
  }
  return(perf.pval)
}
