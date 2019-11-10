#' maxcovr
#'
#' @name maxcovr
#' @useDynLib maxcovr
#' @importFrom Rcpp sourceCpp
#' @importFrom magrittr %>%
NULL

if (getRversion() >= "2.15.1")  utils::globalVariables(c("."))

globalVariables(c("bind_rows",
                  "distance",
                  "d_existing",
                  "facility_chosen",
                  "facility_id",
                  "is_installed",
                  "is_relocated",
                  "is_covered",
                  "key",
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
                  "V1",
                  "V2",
                  "V3"
                  ))
