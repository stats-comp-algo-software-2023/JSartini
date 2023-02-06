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
hiper_glm <- function(design, outcome){
  #TODO: find MLE. Currently returns S3 object containing empty list.
  hglm_out <- list()
  class(hglm_out) <- "hglm"
  return(hglm_out)
}
