library(R6)

RV <- R6Class(
  "RV",
  public = list(
    dist_name = NULL,
    params = list(),
    vals = NULL,
    range = NULL,
    cont = NULL,

    initialize = function(dist_name, params) {
      self$dist_name <- dist_name
      self$params <- params

      # compute attributes
      if (self$dist_name == "normal") {
        self$range <- c(self$params$mean-3*self$params$sd,
                        self$params$mean+3*self$params$sd)
        self$cont <- TRUE
      } else if (self$dist_name == "uniform") {
        self$range <- c(self$params$min, self$params$max)
        self$cont <- TRUE
      } else if (self$dist_name == "binomial") {
        self$range <- c(0, 1)
        self$cont <- FALSE
      }
    },

    generate_samples = function(n) {
      if (self$dist_name == "normal") {
        return(rnorm(n,
                     mean = self$params$mean,
                     sd = self$params$sd))
      } else if (self$dist_name == "uniform") {
        return(runif(n,
                     min = self$params$min,
                     max = self$params$max))
      } else if (self$dist_name == "binomial") {
        return(rbinom(n,
                      size = 1,
                      prob = self$params$prob))
      }
    },

    # randomly draw k knots from the range of the RV
    get_knots = function(k) {
      if (self$cont) {
        # continuous
        return(runif(k, self$range[1], self$range[2]))
      } else {
        # binary
        return(rep(1, times = k))
      }
    }
  )
)
