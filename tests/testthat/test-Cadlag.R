devtools::load_all()
fun_1 <- Cadlag$new(n_vars = 10)
fun_1$gen_formula()
df <- fun_1$gen_samples(100)
