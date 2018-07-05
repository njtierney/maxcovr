#' maxcovr.
#'
#' @name maxcovr
#' @useDynLib maxcovr
#' @importFrom Rcpp sourceCpp
#' @importFrom magrittr %>%
#' @importFrom stats sd
NULL

if (getRversion() >= "2.15.1")  utils::globalVariables(c("."))

globalVariables(c("is_installed",
                  "is_relocated",
                  "is_covered",
                  "lat",
                  "lat.x",
                  "lat.y",
                  "lat_facility",
                  "lat_user",
                  "long",
                  "long.x",
                  "long.y",
                  "long_facility",
                  "long_user",
                  "n",
                  "n_cov",
                  "n_not_cov",
                  "rank_distance",
                  "sd",
                  "user_chosen",
                  "user_id",
                  "distance",
                  "V1",
                  "V2",
                  "V3",
                  "bind_rows",
                  "facility_chosen",
                  "facility_id",
                  "key"
))
