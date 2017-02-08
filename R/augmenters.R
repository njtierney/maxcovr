# augmenters and model summary constructors.
# these functions exist to provide more transparent and re-useable
# functions to provide summaries for maxcovr.
# Soon, they will be implemented throughout maxcovr, but are being tested atm.

#' Nearest wrapper
#'
#' This function provides a little wrapper function for \code{nearest}.
#' Although, adding \code(is_covered) to the model. In the future this might
#' get added to nearest, and this function will become sorta obsolete.
#' For the moment, this function exists because (to me, as least) it is
#' explicit about the input. It is used so that in a cross validation context
#' we can evaluate how well the test data performs against the facilities
#' suggested in the training set.
#'
#' @param all_facilities data.frame All facilities selected in maxcovr model
#' @param test_data data.frame test data (but it could be any `user`-type data)
#' @param distance_threshold numeric
#'
#' @return dataframe containing distances between each test data observation and the nearest facility.
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' mc_cv_relocate_n100_cut %>%
#'   mutate(user_nearest_test = map2(
#'     .x = facilities_selected,
#'     .y = test,
#'     .f = augment_user_tested
#'     ))
#'
#' }
#'
augment_user_tested <- function(all_facilities,
                                test_data,
                                distance_threshold = 100){

    nearest(nearest_df = all_facilities,
            to_df = test_data) %>%
        dplyr::mutate(is_covered = (distance <= distance_threshold))

}

#' Summarise the coverage for users
#'
#' This uses a \code{user} dataframe obtained from something like
#' \code{augment_user_tested}.
#'
#' @param user dataframe of users with distances between each user and the
#'   nearest facility (\code{distance}), and whether this is within the distance
#'   threshold (\code{is_covered}).
#'
#' @return dataframe containing information on the number of users, the number
#'   of events covered, the proportion of events covered, and the distance from
#'   each
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#'summarise_user_cov(augmented_user_test)
#'
# augmented_user_test %>%
#   group_by(area) %>%
#   summarise_user_cov()
#'
#' }
#'
#'
summarise_user_cov <- function(user){

    user %>%
        dplyr::summarise(n_users = n(),
                         n_cov = sum(is_covered),
                         pct_cov = mean(is_covered),
                         dist_avg = mean(distance),
                         dist_sd = sd(distance))

}

#' Find distance from relocated and proposed new sites
#'
#' This takes the proposed sites and the existing sites, with additional
#' information from the model, and then returns a dataframe of all of the
#' existing facilities that were relocated, and provides the distance to the
#' nearest facility, which is presumably the location to which it was relocated
#' to.
#'
#' @param proposed_facility facilities proposed for the model - but this data
#'   has extra information (\code{is_installed}) in it.
#' @param existing_facility facilities existing for the model - but this data
#'   has extra information (\code{is_relocated}) in it.
#'
#' @return dataframe
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' mc_cv_n100_test %>%
#'   mutate(facility_distances = map2(
#'     .x = proposed_facility,
#'     .y = existing_facility,
#'     .f = augment_facility_relocated)) %>%
#'   select(facility_distances) %>%
#'   .[[1]]
#'
#' }
#'
augment_facility_relocated <- function(proposed_facility,
                                       existing_facility){

    nearest(
        nearest_df = {dplyr::filter(proposed_facility,
                                    is_installed == 1)},
        to_df = {dplyr::filter(existing_facility,
                               is_relocated == 1)}
    )
}

# the little tiny cute summaries of proposed facilities


#' Extract the number of facilities relocated.
#'
#' @param existing_facility the facilities originally existing - this will have
#'   to be output from the model, however.
#'
#' @return dataframe containing a single column of
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' mc_cv_n100_test %>%
#'   mutate(n_relocated = map(
#'     .x = existing_facility,
#'     .f = n_relocated)) %>%
#'   select(n_relocated) %>%
#'   .[[1]]
#'
#' }
#'
n_relocated <- function(existing_facility){

    existing_facility %>%
        dplyr::summarise(n_relocated = sum(is_relocated))
}

#' Extract the number of facilities installed
#'
#' Using the model-modified dataframe of \code{proposed_facility}, count the
#' number of events installed.
#'
#' @param proposed_facility dataframe from the mc_model, of facilities proposed
#'   with the additional information about whether the facility was installed
#'   or not - \code{is_installed}
#'
#' @return datafrmae
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' mc_cv_n100_test %>%
#'     mutate(n_installed = map(
#'         .x = proposed_facility,
#'         .f = n_installed
#'     )) %>%
#'     select(n_installed) %>%
#'     .[[1]]
#'
#' }
#'
n_installed <- function(proposed_facility){

    proposed_facility %>%
        dplyr::summarise(n_installed = sum(is_installed))

}


#' Find the average distance from facilities relocated to their final place
#'
#' This takes data from the function \code{augment_facility_relocated}
#' function of the same name and then summarises it to find the average and sd
#' of the distance between the two.
#'
#' @param augment_facility_relocated dataframe from function:
#'   \code{augment_facility_relocated}
#'
#' @return dataframe
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' mc_cv_n100_test %>%
#'     mutate(
#'         facility_distances = map2(
#'             .x = proposed_facility,
#'             .y = existing_facility,
#'             .f = augment_facility_relocated
#'         ),
#'         summary_relocated_dist = map(
#'             .x = facility_distances,
#'             .f = summarise_relocated_dist
#'         )
#'     ) %>%
#'     # select(facility_distances) %>%
#'     select(summary_relocated_dist) %>%
#'     .[[1]]
#'
#' }
#'
summarise_relocated_dist <- function(augment_facility_relocated){

    augment_facility_relocated %>%
        dplyr::summarise(dist_avg = mean(distance),
                         dist_sd = sd(distance))

}

