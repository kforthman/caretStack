# variable importance for repeated nested CV #
varImp_rNCV <- function(rncv.obj){
  names(rncv.obj$var.imp)[4] <- 'importance'
  summ <- setNames(aggregate(importance~variable, data=rncv.obj$var.imp, mean), c('variable','mean'))
  summ <- merge(summ,
                setNames(aggregate(importance~variable, data=rncv.obj$var.imp, sd), c('variable','se')),
                by='variable')
  summ <- summ[order(summ$mean),]
  p <- barplot(summ$mean, horiz=T, names.arg=summ$variable, las=1, xlim=c(0,100))
  segments(summ$mean - summ$se, p, summ$mean + summ$se, p, lwd = 1.5)
  return(summ)
}
