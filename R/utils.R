`%+%` <- function(a, b) paste0(a, b)

get_r_square <- function(y_pred, y_true) {
  return(1 - sum((y_true-y_pred)^2)/sum((y_true-mean(y_true))^2))
}
