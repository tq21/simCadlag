library(purrr)

dist_opt <- c("normal", "uniform")
p_opt <- c(3, 4, 5)

param_list_opt <- list(
  normal = list(mean = c(1, 2, 3, 4, 5),
                variance = c(1, 2, 3)),
  uniform = list(min = c(-1, -2, -3, -4, -5),
                 max = c(1, 2, 3, 4, 5))
)

sim_data_factory <- function(p_opt,
                             dist_opt,
                             param_list_opt,
                             seed = 123) {
  set.seed(seed)

  p <- sample(p_opt, 1)
  selected_dists <- sample(dist_opt, p, replace = TRUE)
  selected_params <- lapply(1:p, function(j) {
    dist_name <- selected_dists[j]
    params <- param_list_opt[[dist_name]]
    return(lapply(params, function(param) sample(param, 1)))
  })
  coeffs <- runif(p+1, -1, 1)

  # function to generate data
  sim_data <- function(n) {
    X <- matrix(nrow = n, ncol = p)
    for (i in 1:p) {
      dist_name <- selected_dists[i]
      params <- selected_params[[i]]
      if (dist_name == "normal") {
        X[, i] <- rnorm(n, mean = params$mean, sd = sqrt(params$variance))
      } else if (dist_name == "uniform") {
        X[, i] <- runif(n, min = params$min, max = params$max)
      }
    }

    Y <- cbind(1, X) %*% coeffs + rnorm(n)

    return(data.frame(X, Y = Y))
  }

  return(sim_data)
}

test <- sim_data_factory(p_opt, dist_opt, param_list_opt)
test(100)
test_2 <- sim_data_factory(p_opt, dist_opt, param_list_opt)
test_2(100)
