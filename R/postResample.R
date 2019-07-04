# For a training set, getTrainPerf() gives the mean of "k" (for k-fold CV) "by.fold" performance
#  measures whereas defaultSummary(pred) gives 'combine fold' measures.
# For consistency purpose (with stacking predictions), I use defaultSummary(pred)
# NOTE: my notation of "train.by.fold" refers to the "testing" folds
# i.e. a full dataset is divided into k parts for "training" and "testing" (outer-loop)
#      each training set is further divided into k parts for parameter optimization (inner loop)
# revise postResamp() to use "traditional' formula in calculating R2
postResample <- function(pred, obs){
  isNA <- is.na(pred)
  pred <- pred[!isNA]
  obs <- obs[!isNA]

  if (!is.factor(obs) && is.numeric(obs))
  {
    if(length(obs) + length(pred) == 0)
    {
      out <- rep(NA, 3)
    } else {

      mse <- mean((pred - obs)^2)
      rsq <- 1 - mse*length(obs)/(var(obs)*(length(obs)-1))
      mae <- mean(abs(pred - obs))

      out <- c(sqrt(mse), rsq, mae)
    }
    names(out) <- c("RMSE", "Rsquared", "MAE")
  } else {
    if(length(obs) + length(pred) == 0)
    {
      out <- rep(NA, 2)
    } else {
      pred <- factor(pred, levels = levels(obs))
      requireNamespaceQuietStop("e1071")
      out <- unlist(e1071::classAgreement(table(obs, pred)))[c("diag", "kappa")]
    }
    names(out) <- c("Accuracy", "Kappa")
  }
  if(any(is.nan(out))) out[is.nan(out)] <- NA
  out
}
