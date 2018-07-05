#' extract_mc_results
#'
#' `extract_mc_results` takes a fitted max_coverage object and returns
#'  useful summary information from the model
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

extract_mc_results <- function(x){

# find the facilities selected ================================================

    mc_facilities_selected <- extract_facility_selected(
        solution_vector = x$lp_solution$solution,
        A_mat = x$A,
        proposed_facilities = x$proposed_facility)

# user_affected ==============================================================

    mc_users_affected <- extract_users_affected(
        A_mat = x$A,
        solution_vector = x$lp_solution$solution,
        user_id = x$user_id,
        users_not_covered = x$user_not_covered)

    # return the users entered into the
    # augmented_users - a dataframe with the original users, but with
    # distance + a few extras added

    mc_augmented_users <- augment_user(
        facilities_selected = mc_facilities_selected,
        existing_facilities = x$existing_facility,
        existing_users = x$existing_user)

    # model_coverage

    mc_model_coverage  <- extract_model_coverage(
        augmented_user = mc_augmented_users,
        distance_cutoff = x$distance_cutoff,
        n_added = x$n_added)

    # existing_coverage

    mc_existing_coverage <- extract_existing_coverage(
        existing_facilities = x$existing_facility,
        existing_users = x$existing_user,
        distance_cutoff = x$distance_cutoff)

    # summary

    # - I dont think this is necessary anymore?

    mc_summary <- bind_rows(
        mc_existing_coverage,
        mc_model_coverage
    )

    # Also need to return all of the information input into the model
    # existing_facility
    # proposed_facility
    # user

    mc_res <- tibble::tibble(
        n_added = list(x$n_added),
        distance_cutoff = list(x$distance_cutoff),
        # existing_facility = list(x$existing_facility),
        # proposed_facility = list(x$proposed_facility),
        # user = list(x$existing_user),
        user_affected = list(mc_users_affected),
        augmented_users = list(mc_augmented_users),
        facility_selected = list(mc_facilities_selected),
        model_coverage = list(mc_model_coverage),
        existing_coverage = list(mc_existing_coverage),
        summary = list(mc_summary),
        model_call = list(x$model_call),
        solution = list(x$lp_solution)
    )

    class(mc_res) <- c("maxcovr",class(mc_res))

    return(mc_res)

}

#
#     # extract results =======================================================
#
#     # get the dimenions from the matrix
#     J <- nrow(x$A)
#     I <- ncol(x$A)
#
#     # which AEDs are to be used
#     facility_solution <- x$lp_solution$solution[1:I]
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
#     facility_selected <- x$proposed_facility %>%
#         dplyr::mutate(facility_id = facility_id) %>%
#         dplyr::filter(facility_id %in% facility_temp$facility_id) %>%
#         # drop facility_id as it is not needed anymore
#         dplyr::select(-facility_id)
#
#     # which OHCAs are affected
#     user_solution <- x$lp_solution$solution[c(I + 1):c(I + J)]
#
#     user_temp <- tibble::tibble(
#         user_id = x$user_id,
#         user_chosen = user_solution) %>%
#         dplyr::filter(user_chosen == 1)
#
#     user_affected <- dplyr::left_join(user_temp,
#                                x$user_not_covered,
#                                by = "user_id")
#         # x$user %>%
#         # mutate(user_id = user_id) %>%
#         # dplyr::filter(user_id %in% user_temp$user_id) %>%
#         # # drop user_id, as it not needed anymore
#         # select(-user_id)
#         # dplyr::filter(event_id %in% user_temp$user_id)
#
# # now to return some more summaries ...
#
# # NOTE: I really should use `nearest`
#     facility_sum_prep <- dplyr::bind_rows(facility_selected,
#                                           x$existing_facility) %>%
#         dplyr::select(lat,long) %>%
#         as.matrix()
#
#     user_sum_prep <- x$existing_user %>%
#         dplyr::select(lat,long) %>%
#         as.matrix()
#
#     dist_sum_df <-
#         maxcovr::nearest_facility_dist(facility = facility_sum_prep,
#                                        user = user_sum_prep) %>%
#         dplyr::as_data_frame() %>%
#         dplyr::rename(user_id = V1,
#                facility_id = V2,
#                distance = V3) %>%
#         dplyr::mutate(is_covered = (distance <= x$distance_cutoff))
#
#     model_coverage <- dist_sum_df %>%
#         dplyr::summarise(n_added = as.numeric(x$n_added),
#                          distance_within = as.numeric(x$distance_cutoff),
#                          n_cov = sum(is_covered),
#                          pct_cov = mean(is_covered),
#                          n_not_cov =  (sum(is_covered == 0)),
#                          pct_not_cov = 1 - mean(is_covered),
#                          dist_avg = mean(distance),
#                          dist_sd = stats::sd(distance))
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
#     summary_coverage = dplyr::bind_rows(existing_coverage,
#                                         model_coverage)
#
#
#         res <- tibble::tibble(
#             facility_selected = list(facility_selected),
#             user_affected = list(user_affected),
#             model_coverage = list(model_coverage),
#             existing_coverage = list(existing_coverage),
#             summary = list(summary_coverage),
#             n_added = list(x$n_added),
#             distance_cutoff = list(x$distance_cutoff),
#             model_call = list(x$model_call)
#         )
#
#             # not really sure if I need to provide the user + facility solution
#             # but perhaps I could provide this in another function to extract
#             # the working parts of the optimisation
#             # user_solution = user_solution,
#             # facility_solution = facility_solution,
#             # facilities_users_merge = facilities_users_merge,
#             #add the variables that were used here to get more info

# res <- c(class(res),"maxcovr_relocation")
# class(res) <- c("maxcovr_relocation")
