#' Extraction of hglm model coefficients
#'
#' @details Makes point estimates of hglm coefficients available.
#'
#' @param object, fitted model output of a hiper_glm() function call (class hglm)
#' @param ..., potential additional arguments required for generic coef
#'
#' @return Named vector containing coefficient point estimates
#'
#' @export
#'
coef.hglm <- function(object, ...){
  if(!("Model_Coefs" %in% names(object))){
    stop("Object does not have coefficients calculated as required.")
  }
  return(object[["Model_Coefs"]])
}

#' Extraction of variance-covariance of hglm model coefficients
#'
#' @details Makes variance-covariance matrix of hglm coefficient estimates available
#'
#' @param object, fitted model output of a hiper_glm() function call (class hglm)
#' @param ..., potential additional arguments required for generic vcov
#'
#' @return Labelled variance-covariance matrix
#'
#' @export
vcov.hglm <- function(object, ...){
  warning("Not yet implemented.")
}

#' Create and display curated output from hglm
#'
#' @details Generates informative output from fitted hglm
#'
#' @param x, fitted model output of a hiper_glm() function call (class hglm)
#' @param ..., potential additional arguments required for generic print
#'
#' @return String displayed
#'
#' @export
print.hglm <- function(x, ...){
  cat("Text output of hiper_glm.")
}
