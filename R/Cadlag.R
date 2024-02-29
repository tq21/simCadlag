#' Cadlag function class
#'
#' @description An `R6` class for randomly generating a Cadlag function.
#' Currently the class is designed to generate a Cadlag function of the form
#' finite linear combination of 0-order basis functions, that is, indicator
#' functions.
#'
#' @export
#'
#' @importFrom R6 R6Class
#' @importFrom purrr map map2 map_dbl map_dfc
#' @importFrom hal9001 make_design_matrix
#' @importFrom utils combn
#'
#' @examples
#' set.seed(123)
#' random_function <- Cadlag$new(n_vars = 3)
#' random_function$gen_formula()
#' df <- random_function$gen_samples(500)
Cadlag <- R6Class(
  "Cadlag",
  public = list(
    #' @field order
    #' Order of each basis function.
    order = NULL,

    #' @field total_var_norm
    #' Total variation norm of the function.
    total_var_norm = NULL,

    #' @field formula
    #' Formula of the function.
    formula = NULL,

    #' @field n_vars
    #' Number of variables in the function.
    n_vars = NULL,

    #' @field coefs
    #' Coefficients of the basis functions.
    coefs = NULL,

    #' @field rv_list
    #' RV (random variable) objects.
    rv_list = NULL,

    #' @field param_list
    #' Parameters for RV objects.
    param_list = NULL,

    #' @field basis_list
    #' Basis functions.
    basis_list = NULL,

    #' @field max_degree
    #' Maximum degree of interaction of the basis functions.
    max_degree = NULL,

    #' @description Initialize a new instance of the Cadlag class.
    #'
    #' @param n_vars An integer for the number of variables in the function.
    initialize = function(n_vars) {
      self$n_vars <- n_vars

      # randomly generate configurations
      self$order <- 0
      self$max_degree <- min(n_vars, 3)
      self$total_var_norm <- runif(1, 0, 100)
    },

    #' @description Generate parameters for RV objects
    #'
    #' @param samp_dists A character vector for the sampled distributions.
    gen_param_list = function(samp_dists) {
      self$param_list <- map(samp_dists, function(x) {
        if (x == "normal") {
          return(list(mean = runif(1, -10, 10), sd = runif(1, 0, 10)))
        } else if (x == "uniform") {
          min_and_max <- runif(2, -10, 10)
          return(list(min = min(min_and_max), max = max(min_and_max)))
        } else if (x == "binomial") {
          return(list(prob = runif(1, 0.2, 0.8)))
        }
      })
    },

    #' @description Generate a list of RV objects
    gen_rv_list = function() {
      dist_names <- c("normal", "uniform", "binomial")

      # draw distributions of RVs and their parameters
      samp_dists <- sample(dist_names, self$n_vars, replace = TRUE)
      self$gen_param_list(samp_dists)

      # initialize RVs based on the distributions and parameters
      self$rv_list <- map2(samp_dists, self$param_list, function(dist, params) {
        return(RV$new(dist, params))
      })
    },

    #' @description Generate intercept and coefficients given the total variation norm
    gen_coefs = function() {
      coefs <- runif(length(self$basis_list)+1, min = -1, max = 1)
      self$coefs <- coefs / sum(abs(coefs)) * self$total_var_norm
    },

    #' @description Generate a formula for Y ~ X
    gen_formula = function() {
      # generate RVs
      self$gen_rv_list()

      # TODO: option to sample a subset of RVs

      # generate a list of variable interactions, up-to max_degree-way
      all_cols <- unlist(lapply(1:self$max_degree, function(g) {
        combn(1:self$n_vars, g, simplify = FALSE)
      }), recursive = FALSE)
      all_cols_samp <- sample(all_cols, sample(1:length(all_cols), 1))

      # sample knots for each basis function
      self$basis_list <- unlist(map(all_cols_samp, function(cols) {
        num_knots <- sample(1:50, 1)
        cutoffs_samp <- map(self$rv_list[cols], function(rv_obj) {
          rv_obj$get_knots(num_knots)
        })

        map(1:num_knots, function(j) {
          cutoffs <- map_dbl(cutoffs_samp, j)
          return(list(cols = cols,
                      cutoffs = cutoffs,
                      orders = rep(self$order, length(cols))))
        })
      }), recursive = FALSE)

      # generate coefficients
      self$gen_coefs()

      # TODO: print some summary statistics after generating a Cadlag function

      # form Y ~ X formula, work on this later, only needed for printing
      # self$formula <-
      #   "Y=" %+% self$coefs[1] %+% "+" %+%
      #   paste(map2(var_idx, knots, function(idx, knot) {
      #     return("I(X_" %+% idx %+% ">=" %+% knot %+%")")
      #     }), collapse = "+")
    },

    #' @description Sample n copies of the data
    #'
    #' @param n An integer for the number of samples to generate.
    gen_samples = function(n) {
      # generate X
      X <- map_dfc(1:length(self$rv_list), function(i) {
        rv_obj <- self$rv_list[[i]]
        tmp <- data.frame(tmp = rv_obj$generate_samples(n))
        names(tmp) <- "X" %+% i
        return(tmp)
      })

      # generate Y
      expanded_mat <- hal9001::make_design_matrix(as.matrix(X), self$basis_list)
      Y <- as.vector(self$coefs[1] + expanded_mat %*% self$coefs[-1] + rnorm(n, 0, 1))

      return(list(X = X, Y = Y))
    }
  )
)
