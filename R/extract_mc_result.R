#' (Internal) Summarise maxcovr model using facility and user information
#'
#' `extract_mc_results` takes a fitted `max_coverage` object and
#'   returns useful summary information from the model. It exists so that the
#'   manipulation functions for the outcomes from the solver have another
#'   home - this makes it easier to maintain this package, and heeds to this
#'   idea of having functions that are specialised. The name of this function
#'   is likely to change in the near future.
#'
#' @param x the fitted model from `max_coverage`.
#'
#' @return a list containing multiple dataframes summarising the model

extract_mc_results <- function(x){

    mc_facilities_selected <- extract_facility_selected(
        solution_vector = x$solution$solution,
        A_mat = x$A,
        proposed_facilities = x$proposed_facility)

    mc_users_affected <- extract_users_affected(
        A_mat = x$A,
        solution_vector = x$solution$solution,
        user_id = x$user_id,
        users_not_covered = x$user_not_covered)

    # return the users entered into the augmented_users - a dataframe with the
    # original users, but with distance + a few extras added

    mc_augmented_users <- augment_user(
        facilities_selected = mc_facilities_selected,
        existing_facilities = x$existing_facility,
        existing_users = x$existing_user)

    mc_model_coverage  <- extract_model_coverage(
        augmented_user = mc_augmented_users,
        distance_cutoff = x$distance_cutoff,
        n_added = x$n_added)

    mc_existing_coverage <- extract_existing_coverage(
        existing_facilities = x$existing_facility,
        existing_users = x$existing_user,
        distance_cutoff = x$distance_cutoff)

    mc_summary <- dplyr::bind_rows(mc_existing_coverage,
                                   mc_model_coverage)

    mc_res <- tibble::tibble(
        n_added = list(x$n_added),
        distance_cutoff = list(x$distance_cutoff),
        user_affected = list(mc_users_affected),
        augmented_users = list(mc_augmented_users),
        facility_selected = list(mc_facilities_selected),
        model_coverage = list(mc_model_coverage),
        existing_coverage = list(mc_existing_coverage),
        summary = list(mc_summary),
        model_call = list(x$model_call),
        solution = list(x$solution)
        )

    class(mc_res) <- c("maxcovr",class(mc_res))

    return(mc_res)

}
