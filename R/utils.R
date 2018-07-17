#' @export
print.maxcovr_relocation <- function(x, ...){

    user_input <- c(
        paste(x$model_call[[1]]),
        x$solver_used[[1]]
        )

    model_input <- c("model_used",names(formals(max_coverage_relocation)))

    cat("\n-----------------------------------------" ,
        "\nModel Fit: maxcovr relocation model",
        "\n-----------------------------------------",
        # I tried, I really did, to use purrr. "
        # purrr::map2(model_input, user_input,
        #             +              sprintf("%s: %s"))
        # Error in sprintf("%s: %s") : too few arguments
        sprintf("\n%s:        %s", model_input[[1]], user_input[[1]]),
        sprintf("\n%s: %s", model_input[[2]], user_input[[2]]),
        sprintf("\n%s: %s", model_input[[3]], user_input[[3]]),
        sprintf("\n%s:              %s", model_input[[4]], user_input[[4]]),
        sprintf("\n%s:   %s", model_input[[5]], user_input[[5]]),
        sprintf("\n%s:      %s", model_input[[6]], user_input[[6]]),
        sprintf("\n%s:     %s", model_input[[7]], user_input[[7]]),
        sprintf("\n%s:        %s", model_input[[8]], user_input[[8]]),
        sprintf("\n%s:            %s", model_input[[9]], user_input[[9]]),
        "\n-----------------------------------------"
    )

}

#' @export
summary.maxcovr_relocation <- function(object, ...){

    cat("\n---------------------------------------",
        "\nModel Fit: maxcovr relocation model",
        "\n---------------------------------------",
        sprintf("\nDistance Cutoff: %sm",
                object$model_coverage[[1]]$distance_within),
        "\nFacilities:",
        "\n    Added:      ",
        paste(deparse(object$model_coverage[[1]]$n_proposed_chosen)),
        "\n    Removed:    ",
        paste(deparse(object$model_coverage[[1]]$n_existing_removed)),
        "\nCoverage (Previous):",
        "\n    # Users: ",
        sprintf("   %s   (%s)",
                object$model_coverage[[1]]$n_cov,
                object$existing_coverage[[1]]$n_cov),
        "\n    Proportion: ",
        sprintf("%s (%s)",
                round(object$model_coverage[[1]]$pct_cov,4),
                round(object$existing_coverage[[1]]$pct_cov,4)
                ),
        "\nDistance (m) to Facility (Previous):",
        sprintf("\n       Avg:      %s (%s)",
                round(object$model_coverage[[1]]$dist_avg,0),
                round(object$existing_coverage[[1]]$dist_avg,0)),
        sprintf("\n       SD:       %s (%s)",
                round(object$model_coverage[[1]]$dist_sd,0),
                round(object$existing_coverage[[1]]$dist_sd,0)),
        "\nCosts:",
        "\n    Total:      ",
        paste(deparse(object$model_coverage[[1]]$total_cost)),
        "\n    Install:    ",
        paste(deparse(object$model_coverage[[1]]$install_cost)),
        "\n    Removal: ",
        paste(deparse(object$model_coverage[[1]]$cost_removal)),
        "\n---------------------------------------"
    )

}

#' @export
print.maxcovr <- function(x, ...){

    user_input <- c(paste(x$model_call[[1]]),
                    "lpSolve")

    model_input <- c("model_used",names(formals(max_coverage)))

    cat("\n-------------------------------------------" ,
        "\nModel Fit: maxcovr fixed location model",
        "\n-------------------------------------------",
        sprintf("\n%s:        %s", model_input[[1]], user_input[[1]]),
        sprintf("\n%s: %s", model_input[[2]], user_input[[2]]),
        sprintf("\n%s: %s", model_input[[3]], user_input[[3]]),
        sprintf("\n%s:              %s", model_input[[4]], user_input[[4]]),
        sprintf("\n%s:   %s", model_input[[5]], user_input[[5]]),
        sprintf("\n%s:           %s", model_input[[6]], user_input[[6]]),
        sprintf("\n%s:            %s", model_input[[7]], user_input[[7]]),
        "\n-------------------------------------------"
    )

}

#' @export
summary.maxcovr <- function(object, ...){

    cat("\n-------------------------------------------" ,
        "\nModel Fit: maxcovr fixed location model",
        "\n-------------------------------------------",
        sprintf("\nDistance Cutoff: %sm",
                object$model_coverage[[1]]$distance_within),
        "\nFacilities:",
        "\n    Added:      ",
        paste(deparse(object$model_coverage[[1]]$n_added)),
        "\nCoverage (Previous):",
        "\n    # Users:    ",
        sprintf("%s    (%s)",
                object$model_coverage[[1]]$n_cov,
                object$existing_coverage[[1]]$n_cov),
        "\n    Proportion: ",
        sprintf("%s (%s)",
                round(object$model_coverage[[1]]$pct_cov,4),
                round(object$existing_coverage[[1]]$pct_cov,4)
        ),
        "\nDistance (m) to Facility (Previous):",
        sprintf("\n    Avg:         %s (%s)",
                round(object$model_coverage[[1]]$dist_avg,0),
                round(object$existing_coverage[[1]]$dist_avg,0)),
        sprintf("\n    SD:          %s (%s)",
                round(object$model_coverage[[1]]$dist_sd,0),
                round(object$existing_coverage[[1]]$dist_sd,0)),
        "\n-------------------------------------------"
    )

}

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
#'
#' @return A tibble with 3 columns: user_id, facility_id, distance, where the
#'   user_id is the identifier for the user, the facility_id is the identifier
#'   for the facility that is closest to that user, and the distance is the
#'   distance in metres from that user to that facility.
#'
nearest_facility_distances <- function(existing_facility,
                                       user){

    existing_facility_cpp <- existing_facility %>%
        dplyr::select(lat, long) %>%
        as.matrix()

    user_cpp <- user %>%
        dplyr::select(lat, long) %>%
        as.matrix()

    dat_nearest_dist <-
        nearest_facility_dist(facility = existing_facility_cpp,
                              user = user_cpp) %>%
        tibble::as_tibble() %>%
        dplyr::rename(user_id = V1,
                      facility_id = V2,
                      distance = V3)

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
#'
#' @return a logical matrix, of 1 if distance
#'   between element i, j is less than or equal to the distance_cutoff, and
#'   0 otherwise.
binary_distance_matrix <- function(facility,
                                   user,
                                   distance_cutoff){

    facility_cpp <- facility %>%
        dplyr::select(lat, long) %>%
        as.matrix()

    user_cpp <- user %>%
        dplyr::select(lat, long) %>%
        as.matrix()

    A <- maxcovr::binary_matrix_cpp(facility = facility_cpp,
                                    user = user_cpp,
                                    distance_cutoff = distance_cutoff)

    return(A)

}
