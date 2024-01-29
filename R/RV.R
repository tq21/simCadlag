library(R6)

RV <- R6Class(
  "RV",
  public = list(
    dist = NULL,
    mean = NULL,
    variance = NULL,
    param_list = list(),
    n = NULL,
    vals = NULL,

    initialize = function(dist, param_list, n) {
      self$dist <- dist
      self$param_list <- param_list
      self$n <- n

      # generate copies
      self$generate_samples()

      # compute mean
      if (self$dist == "normal") {
        self$mean <- self$param_list$mean
      } else if (self$dist == "uniform") {
        self$mean <- (self$param_list$min + self$param_list$max) / 2
      } else {
        stop("Distribution not supported yet!")
      }

      # compute variance
      if (self$dist == "normal") {
        self$variance <- self$param_list$sd^2
      } else if (self$dist == "uniform") {
        self$variance <- ((self$param_list$max - self$param_list$min)^2) / 12
      } else {
        stop("Distribution not supported yet!")
      }
    },

    generate_samples = function() {
      if (self$dist == "normal") {
        self$vals <- rnorm(self$n,
                           mean = self$param_list$mean,
                           sd = self$param_list$sd)
      } else if (self$dist == "uniform") {
        self$vals <- runif(self$n,
                           min = self$param_list$min,
                           max = self$param_list$max)
      } else {
        stop("Distribution not supported yet!")
      }
    }
  )
)

normal_rv <- RV$new("normal", list(mean = 0, sd = 1), 100)
uniformCovar <- RV$new("uniform", list(min = 0, max = 1), 100)
