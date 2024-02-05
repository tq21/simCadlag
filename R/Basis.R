Basis <- R6Class(
  "Basis",
  public = list(
    orders = NULL,
    cols = NULL,
    cutoffs = NULL,

    initialize = function(orders, cols, cutoffs) {
      self$orders <- orders
      self$cols <- cols
      self$cutoffs <- cutoffs
    }
  )
)
