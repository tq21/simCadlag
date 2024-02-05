#' Variable class
#'
#' Class to parameterize a variable.
#'
#' @docType class
#'
#' @importFrom R6 R6Class
#'
#' @export
#'
#' @format \code{\link{R6Class}} object.
#'
#' @section Parameters:
#' - \code{type}: Variable type, either binary or continuous.
#' - \code{range}: Range of the variable.
Variable <- R6Class(
  "Variable",
  public = list(
    type = NULL,
    range = NULL,

    initialize = function(...) {
      arguments <- list(...)

      if (nargs() == 3) {
        self$type <- arguments[["type"]]
        self$range <- arguments[["range"]]
        self$idx <- arguments[["idx"]]
      } else if (nargs() == 0) {
        # randomly generate variable configurations
        self$type <- rbinom(1, 1, 0.5) # 0 for binary, 1 for continuous
        self$range <- c(0, 1) # {0,1} for binary, [0,1] for continuous
      }
    }
  )
)
