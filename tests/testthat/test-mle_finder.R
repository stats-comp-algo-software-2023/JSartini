test_that("linear model gradient/likelihood are appropriate", {
  n_obs <- 32; n_pred <- 4
  data <- simulate_data(n_obs, n_pred, model = 'linear', seed = 1819)
  design <- data$design; outcome <- data$outcome
  resid_var = var(outcome)

  for(i in 1:10){
    beta = rnorm(4)*(2*resid_var)
    num_grad = approx_grad(lm_log_likelihood, beta, design = design, outcome = outcome)
    analytical_grad = lm_loglike_gradient(beta, design, outcome)
    expect_true(are_all_close(num_grad, analytical_grad, abs_tol = 1e-4, rel_tol = 1e-4))
  }

})

test_that("linalg and optim least-sq coincide", {
  n_obs <- 32; n_pred <- 4
  data <- simulate_data(n_obs, n_pred, model = 'linear', seed = 1918)
  design <- data$design; outcome <- data$outcome
  via_linalg_out <- hiper_glm(design, outcome, model = 'linear')
  via_bfgs_out <- hiper_glm(
    design, outcome, model = 'linear', option = list(mle_solver = 'BFGS')
  )
  expect_true(are_all_close(
    coef(via_linalg_out), coef(via_bfgs_out), abs_tol = 1e-2, rel_tol = 1e-2
  ))
})
