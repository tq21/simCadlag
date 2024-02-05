library(hal9001)
library(xgboost)
devtools::load_all()

set.seed(123)
fun_1 <- Cadlag$new(n_vars = 5)
fun_1$gen_formula()
df <- fun_1$gen_samples(1000)
df_test <- fun_1$gen_samples(10000)

# fit HAL
hal_fit <- fit_hal(X = df$X,
                   Y = df$Y,
                   smoothness_orders = 0,
                   family = "gaussian")
pred <- predict(hal_fit, new_data = df_test$X)
r_squared <- get_r_square(pred, df_test$Y)

# fit xgboost
data_matrix <- xgb.DMatrix(data = as.matrix(df$X), label = df$Y)
xgb_fit <- xgb.train(data = data_matrix, nrounds = 100)
pred_xgb <- predict(xgb_fit, as.matrix(df_test$X))
r_squared_xgb <- get_r_square(pred_xgb, df_test$Y)
