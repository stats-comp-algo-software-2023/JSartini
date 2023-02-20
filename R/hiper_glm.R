#' GLM for high-dimensional scenarios
#'
#' @details Fits a GLM to given modelling data of high dimension and returns output object
#'
#' @param design, design matrix for the model
#' @param outcome, outcome vector/matrix for the model
#'
#' @return S3 object containing output of fitting model
#'
#' @export
#'
hiper_glm <- function(design, outcome, model = "linear", option = list()){
  supported_models = c("linear")
  if(!(model %in% supported_models)){
    stop("Specified model is not supported.")
  }
  n = dim(design)[1]; p = dim(design)[2]

  # Logic for linear models specifically
  if(model == "linear"){
    # User-specified solver
    if("mle_solver" %in% names(option)){
      if(option["mle_solver"] == "BFGS"){
        fit = optim(par = rep(0, p), fn = lm_log_likelihood,
                    gr = lm_loglike_gradient, outcome = outcome,
                    design = design, method = "BFGS",
                    control = list(fnscale = -1))
        model_coefs = fit$par
      }
      else{
        stop("Specified solver is not supported.")
      }
    }
    # Default pseudo-inverse solver
    else{
      model_coefs = as.vector(solve(t(design) %*% design, t(design) %*% outcome))
    }
  }

  hglm_out <- list(Model_Coefs = model_coefs)
  class(hglm_out) <- "hglm"
  return(hglm_out)
}
