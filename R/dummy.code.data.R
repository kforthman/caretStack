### a function to convert categorical predictors to dummy coding ###
dummy.code.data <- function(data, var.list, ref.list){
  if (!'psych' %in% installed.packages()){
    install.packages('psych')
  }
  require(psych)
  res <- data
  for (j in 1:length(var.list)){
    dummy <- data.frame(dummy.code(res[, var.list[j]], na.rm=T))
    names(dummy) <- paste0(var.list[j], '.', names(dummy))
    res <- res[, names(res) != var.list[j]]
    res <- cbind(res, dummy)
  }
  res <- res[, !names(res) %in% paste0(var.list, '.',  ref.list)]
  return(res)
}
