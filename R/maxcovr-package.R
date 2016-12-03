#' maxcovr.
#'
#' @name maxcovr
#' @import lpSolve
#' @import dplyr
#' @import tidyr
#' @useDynLib maxcovr
#' @importFrom Rcpp sourceCpp
NULL
options(R_CHECK_FORCE_SUGGESTS_=FALSE)
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))
