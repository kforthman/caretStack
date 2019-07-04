# variable importance for stack models
VarImp <- function(models, final, weight){
  var.imp <- varImp(models[[1]])$importance
  names(var.imp)[1] <- names(models)[1]
  var.imp$variable <- rownames(var.imp)
  var.imp <- var.imp[, c('variable', names(models)[1])]
  for (m in 2:length(models)){
    tmp <- varImp(models[[m]])$importance
    names(tmp) <- names(models)[m]
    var.imp <- cbind(var.imp, tmp)
  }
  var.imp$Stack <- as.matrix(var.imp[, names(models)]) %*% as.matrix(weight)[,1]
  return(var.imp)
}
