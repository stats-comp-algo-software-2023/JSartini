# Function to calculate log-likelihood under linear model (independent errors)
lm_log_likelihood <- function(beta, design, outcome, noise_var = 1){
  dimension = length(outcome)
  log_like = (-1*dimension/2)*log(2 * pi * noise_var) -
                (1/(2*noise_var)) * t(outcome - design %*% beta) %*% (outcome - design %*% beta)
  return(log_like)
}


# Function to calculate gradient of linear model log-likelihood
lm_loglike_gradient <- function(beta, design, outcome, noise_var = 1){
  gradient = -1/(noise_var) * ((t(design) %*% design) %*% beta - t(design) %*% outcome)
  return(as.vector(gradient))
}
