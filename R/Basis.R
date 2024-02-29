#' Basis class
#'
#' @description An `R6` class for a basis function in the style of `hal9001`.
#'
#' @export
#'
#' @importFrom R6 R6Class
Basis <- R6Class(
  "Basis",
  public = list(
    #' @field orders
    #' A numeric vector for the orders of the basis functions.
    orders = NULL,

    #' @field cols
    #' numeric vector for the column indices of the basis functions.
    cols = NULL,

    #' @field cutoffs
    #' A numeric vector for the cutoffs of the basis functions.
    cutoffs = NULL,

    #' Initialize the Basis object
    #'
    #' @param orders A numeric vector for the orders of the basis functions.
    #' @param cols A numeric vector for the column indices of the basis functions.
    #' @param cutoffs A numeric vector for the cutoffs of the basis functions.
    initialize = function(orders, cols, cutoffs) {
      self$orders <- orders
      self$cols <- cols
      self$cutoffs <- cutoffs
    }
  )
)
