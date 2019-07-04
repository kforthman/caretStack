#' No Description.

# For a training set, getTrainPerf() gives the mean of "k" (for k-fold CV) "by.fold" performance
#  measures whereas defaultSummary(pred) gives 'combine fold' measures.
# For consistency purpose (with stacking predictions), I use defaultSummary(pred)

modelPerf <- function(df.obs.pred){
  if (!class(df.obs.pred$obs) %in% c('character', 'factor')) {
    return(defaultSummary(df.obs.pred))
  }
  else {
    resp.lv = levels(df.obs.pred$obs)
    return(multiClassSummary(df.obs.pred, lev = resp.lv))
  }
}
