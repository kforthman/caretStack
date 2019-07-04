# a function to combine results from parallized replication (outer) loop #
comb_rep <- function(LL1, LL2){
  index.outer    <- cbind(LL1$index.outer,  LL2$index.outer )
  stack.wt       <- rbind(LL1$stack.wt,     LL2$stack.wt )
  #stack.model    <- list(LL1$stack.model,   LL2$stack.model)
  y.pred.comb    <- cbind(LL1$y.pred.comb,  LL2$y.pred.comb)
  perf.by.fold   <- rbind(LL1$perf.by.fold, LL2$perf.by.fold)
  perf.comb      <- rbind(LL1$perf.comb ,   LL2$perf.comb )
  perf.train     <- rbind(LL1$perf.train,   LL2$perf.train)
  perf.test      <- rbind(LL1$perf.test ,   LL2$perf.test )
  var.imp        <- rbind(LL1$var.imp,      LL2$var.imp)
  return(list(index.outer = index.outer, stack.wt     = stack.wt    , # stack.model = stack.model,
              y.pred.comb = y.pred.comb, perf.by.fold = perf.by.fold, perf.comb = perf.comb,
              perf.train=perf.train,     perf.test = perf.test,
              var.imp     = var.imp))
}
