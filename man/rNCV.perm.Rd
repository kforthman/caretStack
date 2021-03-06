% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rNCV.perm.R
\name{rNCV.perm}
\alias{rNCV.perm}
\title{rNCV.perm is identical to rNCV except do instead of dopar}
\usage{
rNCV.perm(
  data,
  resp.var,
  ref.lv = NULL,
  nRep,
  nFolds.outer,
  ML.methods,
  control,
  tuneL,
  preProc.opt,
  metric,
  dir.path,
  file.root,
  stack.method = "wt.avg",
  weighted.by = NULL,
  stack.wt = NULL,
  control.stack = NULL
)
}
\description{
2018-08-07 version: use ensemble predictions from rNCV for permutation ###
}
\details{
Parallel at k-fold CV (previous version in Replicates) ##

rNCV.perm is identical to rNCV except
(1) do instead of dopar (parallel for permutation replicates)
(2) res contains only predictive values/probabilities (i.e. y.pred.comb)
(3) use ensemble predictions for model performance in rNCV() and rNCV.perm()
}
