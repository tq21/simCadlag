# .libPaths(c("/global/home/users/skyqiu/R/x86_64-pc-linux-gnu-library/4.2",
#             .libPaths()))

library(spatialHAL)
library(hal9001)
library(R6)
library(cluster)
library(dplyr)
devtools::load_all()

# SIMULATION PARAMETERS
n_var_min <- 1; n_var_max <- 5
n_obs_min <- 200; n_obs_max <- 500
n_test <- 10000
dist_names <- c("normal", "uniform", "binomial")
B <- 1000
results_list <- list()

# SIMULATION
for (b in 1:B) {
  # sample n, n_var
  n <- sample(n_obs_min:n_obs_max, 1)
  n_var <- sample(n_var_min:n_var_max, 1)

  print("Data: " %+% b %+% "; n = " %+% n %+% "; d = " %+% n_var)

  # generate a Cadlag function, sample n obs from the DGP
  cadlag_fun <- Cadlag$new(n_vars = n_var)
  cadlag_fun$gen_formula()
  df <- cadlag_fun$gen_samples(n)
  df_test <- cadlag_fun$gen_samples(n_test)

  # data characteristics
  dist_factor <- factor(map_vec(cadlag_fun$rv_list, ~.x$dist_name),
                        levels = dist_names)
  dist_counts <- as.numeric(table(dist_factor))

  # fit vanilla HAL
  hal_time_0 <- Sys.time()
  hal_fit <- fit_hal(X = df$X,
                     Y = df$Y,
                     smoothness_orders = 0,
                     family = "gaussian")
  hal_time_1 <- Sys.time()
  hal_time <- as.numeric(hal_time_1 - hal_time_0)
  pred <- predict(hal_fit, new_data = df_test$X)
  r_squared <- get_r_square(pred, df_test$Y)
  selected_bases <- length(hal_fit$basis_list[hal_fit$coefs[-1] != 0])
  tol_bases <- length(hal_fit$basis_list)

  # fit spatial HAL (PAM)
  pam_time_0 <- Sys.time()
  pam_obj <- clustering_basis_selector(X = df$X,
                                       algorithm = "pam",
                                       k = round(n/2))
  pam_basis_list <- pam_obj$basis
  hal_pam_fit <- fit_hal(X = df$X,
                         Y = df$Y,
                         smoothness_orders = 0,
                         basis_list = pam_basis_list,
                         family = "gaussian")
  pam_time_1 <- Sys.time()
  pam_time <- as.numeric(pam_time_1 - pam_time_0)
  pred_pam <- predict(hal_pam_fit, new_data = df_test$X)
  r_squared_pam <- get_r_square(pred_pam, df_test$Y)
  selected_bases_pam <- length(hal_pam_fit$basis_list[hal_pam_fit$coefs[-1] != 0])
  tol_bases_pam <- length(hal_pam_fit$basis_list)

  # fit spatial HAL (k-means)
  kmeans_time_0 <- Sys.time()
  kmeans_obj <- clustering_basis_selector(X = df$X,
                                          algorithm = "kmeans",
                                          k = round(n/2))
  kmeans_basis_list <- kmeans_obj$basis
  hal_kmeans_fit <- fit_hal(X = df$X,
                            Y = df$Y,
                            smoothness_orders = 0,
                            basis_list = kmeans_basis_list,
                            family = "gaussian")
  kmeans_time_1 <- Sys.time()
  kmeans_time <- as.numeric(kmeans_time_1 - kmeans_time_0)
  pred_kmeans <- predict(hal_kmeans_fit, new_data = df_test$X)
  r_squared_kmeans <- get_r_square(pred_kmeans, df_test$Y)
  selected_bases_kmeans <- length(hal_kmeans_fit$basis_list[hal_kmeans_fit$coefs[-1] != 0])
  tol_bases_kmeans <- length(hal_kmeans_fit$basis_list)

  # TODO: additional things to collect
  # TIMING, TOTAL VARIATION NORM, TYPE OF VARS

  # collect results
  results_list[[b]] <- list(n = n,
                            d = n_var,
                            total_var_norm = cadlag_fun$total_var_norm,
                            normal = dist_counts[1],
                            uniform = dist_counts[2],
                            binomial = dist_counts[3],
                            r_squared = r_squared,
                            r_squared_pam = r_squared_pam,
                            r_squared_kmeans = r_squared_kmeans,
                            selected_bases = selected_bases,
                            selected_bases_pam = selected_bases_pam,
                            selected_bases_kmeans = selected_bases_kmeans,
                            tol_bases = tol_bases,
                            tol_bases_pam = tol_bases_pam,
                            tol_bases_kmeans = tol_bases_kmeans,
                            hal_time = hal_time,
                            pam_time = pam_time,
                            kmeans_time = kmeans_time)
}

results <- do.call(rbind, lapply(results_list, data.frame, stringsAsFactors = FALSE))
saveRDS(results, "out/results.rds")
