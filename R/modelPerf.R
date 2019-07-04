modelPerf <- function(df.obs.pred){
  if (!class(df.obs.pred$obs) %in% c('character', 'factor')) {
    return(defaultSummary(df.obs.pred))
  }
  else {
    resp.lv = levels(df.obs.pred$obs)
    return(multiClassSummary(df.obs.pred, lev = resp.lv))
  }
}
