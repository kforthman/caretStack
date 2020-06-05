#' Variable importance for repeated nested CV.
#'
#' @param rNCV.obj The rNCV object returned by the function \code{rNCV()}.
#' @export

varImp_rNCV <- function(rNCV.obj){
  names(rNCV.obj$var.imp)[4] <- 'importance'
  summ <- setNames(aggregate(importance~variable, data=rNCV.obj$var.imp, mean), c('variable','mean'))
  summ <- merge(summ,
                setNames(aggregate(importance~variable, data=rNCV.obj$var.imp, sd), c('variable','se')),
                by='variable')
  summ <- summ[order(summ$mean),]
  return(summ)
}
