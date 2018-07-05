#' extract_mc_results_relocation
#'
#' `extract_mc_results_relocation` takes a fitted max_coverage object and
#'   returns useful summary information from the model, specifically for the
#'   relocation method.
#'
#' @description extract_mc_results exists so that the manipulation functions for
#'   the outcomes from the lp solver have another home - this makes it easier to
#'   maintain this package, and heeds to this idea of having functions that are
#'   specialised. The name of this function is likely to change in the near
#'   future.
#'
#' @param x the fitted model from max_coverage
#'
#' @return a list containing multiple dataframes summarising the model

extract_mc_results_relocation <- function(x){

    if (x$solver == "glpk") {

        solution <- x$solution$solution

    } else if (x$solver == "lpSolve") {

        solution <- x$solution$solution

    }

    if (x$solver == "gurobi") {

        solution <- x$solution$x

    }

        J <- nrow(x$A)
        I <- ncol(x$A)

        # how many are in existence?
        n_existing <- nrow(x$existing_facility)

        # how many are proposed?
        n_proposed <- nrow(x$proposed_facility)

        # total number of facilities?
        n_facilities <- (nrow(x$existing_facility) + nrow(x$proposed_facility))

        # how many of the existing ones were removed?
        # n_existing_removed <- sum(x$solution$solution[1:n_existing] == 0)
        n_existing_removed <- sum(solution[1:n_existing] == 0)

        # how many additional proposed ones were selected?
        n_existing_1 <- n_existing + 1

        # how many of the proposed were chosen?
        n_proposed_chosen <- sum(solution[n_existing_1:n_facilities])

        # # how many from the existing locations were moved?
        # n_existing_removed <- n_existing - n_existing_selected

        # which ones were moved?
        which_existing_removed <-
            x$existing_facility[which(solution[1:n_existing] == 0), ]

        # maybe there is now total coverage?
        # create bits to get the right vector size out for the users affected
        n_bit_3 <- n_existing + n_proposed + 1

        n_bit_4 <- length(solution)

        # number of users affected
        n_users_affected <- sum(solution[n_bit_3:n_bit_4])

        # key pieces of information
        # n_proposed_chosen
        # n_existing_removed
        # which_existing_removed

        # which AEDs are to be used
        facility_solution <- solution[1:I]

        facility_id <- readr::parse_number(colnames(x$A))

        # user_id <- x$user_id

        # which facilities are selected?
        facility_temp <- tibble::tibble(
            # get the facility ids
            facility_id = facility_id,
            facility_chosen = facility_solution) %>%
            dplyr::filter(facility_chosen == 1)

        facility_selected <- dplyr::bind_rows(x$existing_facility,
                                              x$proposed_facility) %>%
            dplyr::mutate(facility_id = facility_id) %>%
            dplyr::filter(facility_id %in% facility_temp$facility_id) %>%
            # drop facility_id as it is not needed anymore
            dplyr::select(-facility_id)

        # which OHCAs are affected
        user_solution <- solution[c(I + 1):c(I + J)]

        user_temp <- tibble::tibble(
            user_id = x$user_id,
            user_chosen = user_solution) %>%
            dplyr::filter(user_chosen == 1)

        user_affected <- dplyr::left_join(user_temp,
                                          # x$user_not_covered,
                                          x$existing_user,
                                          by = "user_id")

        # now to return some more summaries ...

        # NOTE: I really should use `nearest`
        facility_sum_prep <- dplyr::bind_rows(facility_selected,
                                              x$existing_facility) %>%
            dplyr::select(lat, long) %>%
            as.matrix()

        user_sum_prep <- x$existing_user %>%
            dplyr::select(lat, long) %>%
            as.matrix()

        dist_sum_df <- maxcovr::nearest_facility_dist(facility = facility_sum_prep,
                                                      user = user_sum_prep) %>%
            dplyr::as_data_frame() %>%
            dplyr::rename(user_id = V1,
                          facility_id = V2,
                          distance = V3) %>%
            dplyr::mutate(is_covered = (distance <= x$distance_cutoff))

        model_coverage <-  dist_sum_df %>%
            dplyr::summarise(
                total_cost = as.numeric(x$cost_total),
                install_cost = as.numeric(x$cost_install),
                cost_removal = as.numeric(x$cost_removal),
                n_proposed_chosen = n_proposed_chosen,
                n_existing_removed = n_existing_removed,
                # n_added = as.numeric(x$n_added),
                distance_within = as.numeric(x$distance_cutoff),
                n_cov = sum(is_covered),
                pct_cov = (sum(is_covered) / nrow(.)),
                # really, these are not needed
                n_not_cov =  (sum(is_covered == 0)),
                pct_not_cov = (sum(is_covered == 0) / nrow(.)),
                dist_avg = mean(distance),
                dist_sd = stats::sd(distance))

        # add the original coverage
        existing_coverage <- x$existing_facility %>%
            nearest(x$existing_user) %>%
            dplyr::mutate(is_covered = (distance <= x$distance_cutoff)) %>%
            dplyr::summarise(
                # n_added = 0,
                distance_within = as.numeric(x$distance_cutoff),
                n_cov = sum(is_covered),
                pct_cov = (sum(is_covered) / nrow(.)),
                n_not_cov =  (sum(is_covered == 0)),
                pct_not_cov = (sum(is_covered == 0) / nrow(.)),
                dist_avg = mean(distance),
                dist_sd = stats::sd(distance))

        # add a summary coverage
        summary_coverage <- dplyr::bind_rows(existing_coverage,
                                             model_coverage)

        # which proposed facilities had a facility installed?
        is_installed_prep <- solution[n_existing_1:n_facilities]

        # update proposed facilities with this info
        x$proposed_facility <- x$proposed_facility %>%
            dplyr::mutate(is_installed = is_installed_prep)

        # which existing facilities were relocated?
        is_relocated <- !solution[1:n_existing]

        # update existing facilities with information about relocation
        x$existing_facility <- x$existing_facility %>%
            dplyr::mutate(is_relocated = is_relocated)

        # which users were affected?
        is_covered <- solution[n_bit_3:n_bit_4]

        # update users with information about relocation
        x$existing_user <- x$existing_user %>%
            dplyr::mutate(is_covered = is_covered)

        res <- tibble::tibble(

            # augmented information about each incoming dataframe
            user = list(x$existing_user),
            existing_facility = list(x$existing_facility),
            proposed_facility = list(x$proposed_facility),

            # basically existing_facility and proposed facility
            facilities_selected = list(facility_selected),

            # simple summary info
            model_coverage = list(model_coverage),
            existing_coverage = list(existing_coverage),
            summary = list(summary_coverage),

            # model_call stuff
            solution_vector = list(solution),
            total_cost = list(x$cost_total),
            distance_cutoff = list(x$distance_cutoff),
            solver_used = list(x$solver),
            model_call = list(x$model_call)

            # # things to recover full data - no longer needed
            # is_covered = list(is_covered),
            # is_installed = list(is_installed),
            # is_relocated = list(is_relocated),

            # # data input
            # user = list(user),
            # proposed_facility = list(proposed_facility),
            # existing_facility = list(existing_facility)

            # #unlikely to need again
            # Pretty sure this can be retrieved from the above information.
            # user_affected = list(user_affected),
            # which_existing_removed = list(which_existing_removed),

        )
        # not really sure if I need to provide the user + facility solution
        # but perhaps I could provide this in another function to extract
        # the working parts of the optimisation
        # user_solution = user_solution,
        # facility_solution = facility_solution,
        # facilities_users_merge = facilities_users_merge,
        #add the variables that were used here to get more info

        # res <- c(class(res),"maxcovr_relocation")
        class(res) <- c("maxcovr_relocation",class(res))
        # class(res) <- c("maxcovr_relocation")

        return(res)


}


# - old code
#     # using results extract_mc_result
#     # in the future this will give the output of max_coverage a class
#     # and then extract_mc_result will be an S3 method
#     # where the output in this case of relocation will be something like
#     # "mc_relocation"
#     # as opposed to the other one, which might be "mc_not_relocate", or something.
#
#     # x <- mc_relocate
#     # get the dimenions from the matrix
#     J <- nrow(x$A)
#     I <- ncol(x$A)
#
#
#     # how many are in existence?
#     n_existing <- nrow(x$existing_facility)
#
#     # how many are proposed?
#     n_proposed <- nrow(x$proposed_facility)
#
#     # total number of facilities?
#     n_facilities <- (nrow(x$existing_facility) + nrow(x$proposed_facility))
#
#     # x$lp_solution$solution[1:n_existing]
#
#     # how many were selected?
#     # n_existing_selected <- sum(x$lp_solution$solution[1:n_existing])
#     n_existing_selected <- sum(x$solution$solution[1:n_existing])
#
#
#     # how many from the existing locations were moved?
#     n_existing_removed <- n_existing - n_existing_selected
#
#     # which ones were moved?
#     which_existing_removed <-
#         x$existing_facility[which(x$solution$solution[1:n_existing] == 0),]
#
#     # how many additional proposed ones were selected?
#     # n_bit_1 <- n_existing+1
#     n_existing_1 <- n_existing+1
#     # n_bit_2 <- n_existing+n_proposed
#
#     #
#     n_proposed_chosen <- sum(x$solution$solution[n_existing_1:n_facilities])
#
#
#     # which proposed facilities had a facility installed?
#     is_installed <- x$solution$solution[n_existing_1:n_facilities]
#
#     # which existing facilities were relocated?
#     is_relocated <- !x$solution$solution[1:n_existing]
#
#     # which users were affected?
#     is_covered <- x$solution$solution[n_bit_3:n_bit_4]
#
#     # maybe there is now total coverage?
#
#     n_bit_3 <- n_existing+n_proposed+1
#
#     n_bit_4 <- length(x$solution$solution)
#
#     # number of users affected
#     n_users_affected <- sum(x$solution$solution[n_bit_3:n_bit_4])
#     is_covered <- x$solution$solution[n_bit_3:n_bit_4]
#
#
#     # 693 things are covered.
#     # which ones?
#     # and how do I assess this? - it's been assesed later in the code
#
#     # key pieces of information
#     # n_proposed_chosen
#     # n_existing_removed
#     # which_existing_removed
#
#     # back to old summary method ---------------------------------------------------
#
#     # ... surprisingly (!) this works with very few changes  good job, past nick!
#
#     # which AEDs are to be used
#     facility_solution <- x$solution$solution[1:I]
#
#     facility_id <- readr::parse_number(colnames(x$A))
#
#     # user_id <- x$user_id
#
#     # which facilities are selected?
#     facility_temp <- tibble::tibble(
#         # get the facility ids
#         facility_id = facility_id,
#         facility_chosen = facility_solution) %>%
#         dplyr::filter(facility_chosen == 1)
#
#     facility_selected <- dplyr::bind_rows(x$existing_facility,
#                                           x$proposed_facility) %>%
#         dplyr::mutate(facility_id = facility_id) %>%
#         dplyr::filter(facility_id %in% facility_temp$facility_id) %>%
#         # drop facility_id as it is not needed anymore
#         dplyr::select(-facility_id)
#
#     # which OHCAs are affected
#     user_solution <- x$solution$solution[c(I+1):c(I+J)]
#
#     user_temp <- tibble::tibble(
#         user_id = x$user_id,
#         user_chosen = user_solution) %>%
#         dplyr::filter(user_chosen == 1)
#
#     user_affected <- dplyr::left_join(user_temp,
#                                       # x$user_not_covered,
#                                       x$existing_user,
#                                       by = "user_id")
#
#     # x$user %>%
#     # mutate(user_id = user_id) %>%
#     # dplyr::filter(user_id %in% user_temp$user_id) %>%
#     # # drop user_id, as it not needed anymore
#     # select(-user_id)
#     # dplyr::filter(event_id %in% user_temp$user_id)
#
#     # now to return some more summaries ...
#
#     # NOTE: I really should use `nearest`
#     facility_sum_prep <- dplyr::bind_rows(facility_selected,
#                                           x$existing_facility) %>%
#         dplyr::select(lat,long) %>%
#         as.matrix()
#
#     user_sum_prep <- x$existing_user %>%
#         dplyr::select(lat,long) %>%
#         as.matrix()
#
#     dist_sum_df <- maxcovr::nearest_facility_dist(facility = facility_sum_prep,
#                                                   user = user_sum_prep) %>%
#         dplyr::as_data_frame() %>%
#         dplyr::rename(user_id = V1,
#                       facility_id = V2,
#                       distance = V3) %>%
#         dplyr::mutate(is_covered = (distance <= x$distance_cutoff))
#
#     model_coverage <- dist_sum_df %>%
#         dplyr::summarise(
#             total_cost = as.numeric(x$cost_total),
#             install_cost = as.numeric(x$cost_install),
#             cost_removal = as.numeric(x$cost_removal),
#             n_proposed_chosen = n_proposed_chosen,
#             n_existing_removed = n_existing_removed,
#             n_added = as.numeric(x$n_added),
#             distance_within = as.numeric(x$distance_cutoff),
#             n_cov = sum(is_covered),
#             pct_cov = (sum(is_covered) / nrow(.)),
#             # really, these are not needed
#             n_not_cov =  (sum(is_covered == 0)),
#             pct_not_cov = (sum(is_covered == 0) / nrow(.)),
#             dist_avg = mean(distance),
#             dist_sd = stats::sd(distance))
#
#     # add the original coverage
#     existing_coverage <- x$existing_facility %>%
#         nearest(x$existing_user) %>%
#         dplyr::mutate(is_covered = (distance <= x$distance_cutoff)) %>%
#         dplyr::summarise(n_added = 0,
#                          distance_within = as.numeric(x$distance_cutoff),
#                          n_cov = sum(is_covered),
#                          pct_cov = (sum(is_covered) / nrow(.)),
#                          n_not_cov =  (sum(is_covered == 0)),
#                          pct_not_cov = (sum(is_covered == 0) / nrow(.)),
#                          dist_avg = mean(distance),
#                          dist_sd = stats::sd(distance))
#
#     # add a summary coverage
#     summary_coverage <- dplyr::bind_rows(existing_coverage,
#                                          model_coverage)
#
#     res <- tibble::tibble(
#         facility_selected = list(facility_selected),
#         user_affected = list(user_affected),
#         which_existing_removed = list(which_existing_removed),
#         model_coverage = list(model_coverage),
#         existing_coverage = list(existing_coverage),
#         summary = list(summary_coverage),
#         total_cost = list(x$cost_total),
#         distance_cutoff = list(x$distance_cutoff),
#         model_call = list(x$model_call),
#         solution_vector = as.logical(x$solution$solution),
#         is_covered = as.logical(is_covered),
#         is_installed = as.logical(is_installed),
#         is_relocated = as.logical(is_relocated)
#     )
#     # not really sure if I need to provide the user + facility solution
#     # but perhaps I could provide this in another function to extract
#     # the working parts of the optimisation
#     # user_solution = user_solution,
#     # facility_solution = facility_solution,
#     # facilities_users_merge = facilities_users_merge,
#     #add the variables that were used here to get more info
#
#     # res <- c(class(res),"maxcovr_relocation")
#     class(res) <- c("maxcovr_relocation",class(res))
#     # class(res) <- c("maxcovr_relocation")
#
#     return(res)
#
#
# } # end the function
