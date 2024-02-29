#' RV (random variable) class
#'
#' @description An `R6` class for randomly generating a random variable.
#' Currently only normal, uniform, and binomial distributions are supported.
#'
#' @export
#'
#' @importFrom R6 R6Class
#' @importFrom stats rnorm runif rbinom
#'
#' @examples
#' set.seed(123)
#' normal_rv <- RV$new(dist_name = "normal", params = list(mean = 0, sd = 1))
#' normal_rv$generate_samples(5)
RV <- R6Class(
  "RV",
  public = list(
    #' @field dist_name
    #' Name of the distribution.
    dist_name = NULL,

    #' @field params
    #' Parameters for the distribution.
    params = list(),

    #' @field vals
    #' Realizations of the RV.
    vals = NULL,

    #' @field range
    #' Range of the RV.
    range = NULL,

    #' @field cont
    #' Whether the RV is continuous.
    cont = NULL,

    #' @description Initialize a new instance of the RV class.
    #'
    #' @param dist_name A string for the name of the distribution.
    #' @param params A list of parameters for the distribution.
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

    #' @description Generate samples from the RV.
    #'
    #' @param n An integer for the number of samples to generate.
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

    #' @description Randomly draw k knots from the range of the RV
    #'
    #' @param k An integer for the number of knots to draw.
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
