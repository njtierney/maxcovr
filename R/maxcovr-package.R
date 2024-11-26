#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom Rcpp sourceCpp
#' @useDynLib maxcovr, .registration = TRUE
## usethis namespace: end
NULL

utils::globalVariables(
    c(
        "...1",
        "...2",
        "...3",
        "d_existing",
        "distance",
        "facility_chosen",
        "facility_id",
        "is_covered",
        "is_installed",
        "is_relocated",
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
        "user_id"
    )
)
