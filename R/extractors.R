# These functions extract results from solvers in maxcovr fixed locations

#' Extract Selected Facilities
#'
#' This takes the linear programming solution, the A matrix, and the proposed
#'   facilities. It returns a tibble, which contains the facilities chosen from
#'   the proposed facilities.
#'
#' @param solution_vector vector from lp_solution$solution
#' @param A_mat The "A" matrix from the solver
#' @param proposed_facilities Dataframe of proposed facilities
#'
#' @return dataframe of selected facilities
#'
#' @examples
#'
#' # assuming that you've run max_coverage using lpSolve, then you
#' # will save the model output before the extraction process
#' # as `x`.
#' \dontrun{
#' mc_facilities_selected <- extract_facility_selected(
#'   solution_vector = x$lp_solution$solution,
#'   A_mat = x$A,
#'   proposed_facilities = x$proposed_facility)
#'   }
#'
extract_facility_selected <- function(solution_vector,
                                      A_mat,
                                      proposed_facilities){

    # the number of facilities, is given by the number of columns in A.
    I <- ncol(A_mat)

    facility_solution <- solution_vector[1:I]

    facility_id <- readr::parse_number(colnames(A_mat))

    # Which facilities were selected -------------------------------------------
    facility_temp <- tibble::tibble(facility_id = facility_id,
                                    facility_chosen = facility_solution) |>
        dplyr::filter(facility_chosen == 1)

    # join these back on.
    facility_selected <- proposed_facilities |>
        dplyr::mutate(facility_id = facility_id) |>
        dplyr::filter(facility_id %in% facility_temp$facility_id) |>
        # drop facility_id as it is not needed anymore
        dplyr::select(-facility_id)

    return(facility_selected)

}

#' Extract users affected
#'
#' Extract additional users affected by new coverage from the new facilities
#'
#' @param A_mat A matrix
#' @param solution_vector The vector of solutions
#' @param user_id The IDs of the individuals
#' @param users_not_covered those users not covered by original AEDs
#'
#' @return tibble taken from `users`, those who are affectd by new placements
#'
#' @examples
#'
#' \dontrun{
#' extract_users_affected(
#'     A_mat = x$A,
#'     solution_vector = x$lp_solution$solution,
#'     user_id = x$user_id,
#'     users_not_covered = x$user_not_covered)
#'     }
#'
extract_users_affected <- function(A_mat,
                                   solution_vector,
                                   user_id,
                                   users_not_covered){

    I <- ncol(A_mat)
    J <- nrow(A_mat)

    user_solution <- solution_vector[c(I + 1):c(I + J)]

    user_temp <- tibble::tibble(user_id = user_id,
                                user_chosen = user_solution) |>
        dplyr::filter(user_chosen == 1)

    user_affected <- dplyr::left_join(user_temp,
                                      users_not_covered,
                                      by = "user_id")

    return(user_affected)

}

#' Augment users data; add useful information
#'
#' This returns the `user` dataframe, with added columns containing distance
#'   between that user and a given facility - IDs are generated for IDs and
#'   facilities that correspond to their row number.
#'
#' @param facilities_selected dataframe of facilities selected, obtained from
#'   `extract_facility_selected`
#' @param existing_facilities existing facilities
#' @param existing_users existing users
#'
#' @return tibble of users, with distances between each user and facility
#'
#' @examples
#'
#' \dontrun{
#'
#' mc_facilities_selected <-extract_facility_selected(
#'   solution_vector = x$lp_solution$solution,
#'   A_mat = x$A,
#'   proposed_facilities = x$proposed_facility)
#'
#' augmented_users <- augment_user(
#'     facilities_selected = mc_facilities_selected,
#'     existing_facilities = mc_cv_fit_n20_test_1$existing_facility,
#'     existing_users = mc_cv_fit_n20_test_1$existing_user
#'     )
#' }
#'
augment_user <- function(facilities_selected,
                         existing_facilities,
                         existing_users){

    # bind the selected and existing facilities together
    all_facilities <- dplyr::bind_rows({
        facilities_selected[ , c("lat", "long")] |>
            dplyr::mutate(type = "selected")
    },{
        existing_facilities[ , c("lat", "long")] |>
            dplyr::mutate(type = "existing")
    })

    # now return the distances etc
    augmented_users <- nearest(all_facilities,
                               existing_users)

    return(augmented_users)

}

#' Extract a one-row summary of the model coverage
#'
#' This function takes the users information, the distance cutoff, and the
#'   number of facilities added, and then returns a one-row dataframe containing
#'   summary information about the coverage.
#'
#' @param augmented_user dataframe obtained from `augment_user()`
#' @param distance_cutoff numeric of the distance cutoff
#' @param n_added numeric of the number of facilities added
#'
#' @return tibble of summary coverage info
#'
#' @examples
#'
#' \dontrun{
#'
#' augmented_users <- augment_user(
#'     facilities_selected = mc_facilities_selected,
#'     existing_facilities = x$existing_facility,
#'     existing_users = x$existing_user)
#'
#' extract_model_coverage(
#'     augmented_user = augmented_users,
#'     distance_cutoff = x$distance_cutoff,
#'     n_added = x$n_added)
#' }
#'

extract_model_coverage <- function(augmented_user,
                                   distance_cutoff,
                                   n_added){

    augmented_user |>
        dplyr::mutate(is_covered = (distance <= distance_cutoff)) |>
        dplyr::summarise(n_added = as.numeric(n_added),
                         distance_within = as.numeric(distance_cutoff),
                         n_cov = sum(is_covered),
                         pct_cov = mean(is_covered),
                         n_not_cov =  (sum(is_covered == 0)),
                         pct_not_cov = 1 - mean(is_covered),
                         dist_avg = mean(distance),
                         dist_sd = stats::sd(distance))

}

#' Extract the existing coverage
#'
#' @param existing_facilities the existing facilities
#' @param existing_users the existing users
#' @param distance_cutoff the distance cutoffs
#'
#' @return tibble of existing coverage
#'
#' @examples
#'
#' \dontrun{
#' extract_existing_coverage(existing_facilities = x$existing_facility,
#'    existing_users = x$existing_user,
#'    distance_cutoff = x$distance_cutoff)
#'    }

extract_existing_coverage <- function(existing_facilities,
                                      existing_users,
                                      distance_cutoff){

    existing_coverage <- nearest(existing_facilities,
                                 existing_users) |>
        dplyr::mutate(is_covered = (distance <= distance_cutoff)) |>
        # very similar summary function is called here and above,
        # ideally this should be written up as a function
        # it is already written up as `summarise_coverage`
        dplyr::summarise(n_added = 0,
                         distance_within = as.numeric(distance_cutoff),
                         n_cov = sum(is_covered),
                         pct_cov = mean(is_covered),
                         n_not_cov =  (sum(is_covered == 0)),
                         pct_not_cov = 1 - mean(is_covered),
                         dist_avg = mean(distance),
                         dist_sd = stats::sd(distance))


    return(existing_coverage)
}
