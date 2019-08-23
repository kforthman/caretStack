#' Check NaN for Data.Frame
#'
#' Used for performance in training set.
#'
#' @inheritParams base::is.finite
#' @examples
#' data <- data.frame(
#'                     x = c(NaN, NaN, NaN),
#'                     y = c("a", "b", "c"),
#'                     z = c(NaN, 1, NaN),
#'                     r = c(NaN, "a", NaN)
#'                   )
#' is.nan.data.frame(data)

is.nan.data.frame <- function(x){
  do.call(cbind, lapply(x, is.nan)) # do.call is good for combining list of matricies
}
