#' Test if the object is a maxcovr object
#'
#' @param x An object
#' @return `TRUE` if the object inherits from the `maxcovr` class.
#'
#' @export
is.maxcovr <- function(x) {

    inherits(x, "maxcovr")

}

#' Test if the object is a maxcovr_relocation object
#'
#' @param x An object
#' @return `TRUE` if the object inherits from the `maxcovr_relocation` class.
#' @export
is.maxcovr_relocation <- function(x) {

    inherits(x, "maxcovr_relocation")

}

#' (Internal) Calculate the nearest facility distances
#'
#' This function is a wrapper for the similarly named, `nearest_facility_dist`
#'   function used inside `max_coverage` to calculate distances
#'   so that the nearest facilities can be found.
#'
#' @param existing_facility dataframe of existing facilities
#' @param user dataframe of users to place facilities to cover
#' @keywords internal
#'
#' @return A tibble with 3 columns: user_id, facility_id, distance, where the
#'   user_id is the identifier for the user, the facility_id is the identifier
#'   for the facility that is closest to that user, and the distance is the
#'   distance in metres from that user to that facility.
#'
nearest_facility_distances <- function(existing_facility,
                                       user){

    existing_facility_cpp <- mc_mat_prep(existing_facility)

    user_cpp <- mc_mat_prep(user)

    dat_nearest_dist <-
        nearest_facility_dist(facility = existing_facility_cpp,
                              user = user_cpp) |>
        tibble::as_tibble(.name_repair = "unique_quiet") |>
        dplyr::rename(user_id = `...1`,
                      facility_id = `...2`,
                      distance = `...3`)

    return(dat_nearest_dist)

}

#' (Internal) Create a binary distance matrix
#'
#' This is a wrapper function that returns a logical matrix, of 1 if distance
#'   between element i, j is less than or equal to the distance_cutoff, and
#'   0 otherwise.
#'
#' @param facility data.frame of facilities
#' @param user data.frame of users
#' @param distance_cutoff integer of distance to use for cutoff
#' @param d_proposed_user Option distance matrix between proposed facilities and
#' users (see Examples).
#' @keywords internal
#'
#' @return a logical matrix, of 1 if distance
#'   between element i, j is less than or equal to the distance_cutoff, and
#'   0 otherwise.
binary_distance_matrix <- function(facility,
                                   user,
                                   distance_cutoff,
                                   d_proposed_user = NULL){

    if (is.null (d_proposed_user)){
        facility_cpp <- mc_mat_prep(facility)

        user_cpp <- mc_mat_prep(user)

        A <- binary_matrix_cpp(facility = facility_cpp,
                               user = user_cpp,
                               distance_cutoff = distance_cutoff)
    } else {
        # reduce d_proposed_user down to submitted `user_not_covered`:
        d_proposed_user <- d_proposed_user[, user$user_id]
        d_proposed_user[is.na (d_proposed_user)] <-
            max (d_proposed_user, na.rm = TRUE)
        A <- t (d_proposed_user < distance_cutoff)
    }

    return(A)

}

#' (Internal) Create a dataframe of the users not covered
#'
#' @param existing_facility data.frame of existing facilities
#' @param user  data.frame of existing users
#' @param distance_cutoff integer of distance cutoff
#' @param d_existing_user Optional distance matrix between existing facilities
#' and users.
#' @keywords internal
#'
#' @return data.frame of those users not covered by current facilities
find_users_not_covered <- function(existing_facility,
                                   user,
                                   distance_cutoff,
                                   d_existing_user = NULL){


    if (is.null (d_existing_user)){
        # make nearest dist into dataframe
        dat_nearest_no_cov <- nearest_facility_distances(
            existing_facility = existing_facility,
            user = user) |>
            # leave only those not covered
            dplyr::filter(distance > distance_cutoff)

    } else {
        if (nrow (d_existing) != nrow (existing_facility) |
            ncol (d_existing) != nrow (user))
            stop ("'d_existing_user' must have same number of rows as 'user',",
                  " and same number of columns as 'existing_facility'")

        d_existing_user[is.na(d_existing_user)] <-
            max(d_existing_user, na.rm = TRUE)
        index <- which(apply(d_existing_user, 2, min) > distance_cutoff)
        nearest_facility <- t(apply(d_existing_user[, index], 2,
                                      function(i) c(which.min(i), min(i))))
        dat_nearest_no_cov <- tibble::tibble(user_id = index,
                                    facility_id = nearest_facility[, 1],
                                    distance = nearest_facility[, 2])
    }
    user_not_covered <- dplyr::left_join(dat_nearest_no_cov,
                                         user,
                                         by = "user_id")

    return(user_not_covered)
}



mc_mat_prep <- function(data){
    as.matrix(data[ , c("lat", "long")])
}
