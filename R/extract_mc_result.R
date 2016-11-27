#' extract_mc_results
#'
#' \code{extract_mc_results} takes a fitted max_coverage object and returns useful summary information from the model
#'
#' @description extract_mc_results exists so that the manipulation functions for the outcomes from the lp solver have another home - this makes it easier to maintain this package, and heeds to this idea of having functions that are specialised. The name of this function is likely to change in the near future.
#'
#' @param x the fitted model from max_coverage
#'
#' @return a list containing multiple dataframes summarising the model

extract_mc_results <- function(x){

    # extract results ---------------------------------------------------------

    # get the dimenions from the matrix
    J <- nrow(x$A)
    I <- ncol(x$A)

    # which AEDs are to be used
    facility_solution <- x$lp_solution$solution[1:I]

    facility_id <- readr::parse_number(colnames(x$A))

    user_id <- x$user_id

    # which facilities are selected?
    facility_temp <- tibble::tibble(
        # get the facility ids
        facility_id = facility_id,
        facility_chosen = facility_solution) %>%
        dplyr::filter(facility_chosen == 1)

    facility_selected <- x$facility %>%
        mutate(facility_id = facility_id) %>%
        dplyr::filter(facility_id %in% facility_temp$facility_id) %>%
        # drop facility_id as it is not needed anymore
        select(-facility_id)

    # which OHCAs are affected
    user_solution <- x$lp_solution$solution[c(I+1):c(I+J)]

    user_temp <- tibble::tibble(
        user_id = x$user_id,
        user_chosen = user_solution) %>%
        dplyr::filter(user_chosen == 1)

    user_affected <- x$user %>%
        mutate(user_id = user_id) %>%
        dplyr::filter(user_id %in% user_temp$user_id) %>%
        # drop user_id, as it not needed anymore
        select(-user_id)
        # dplyr::filter(event_id %in% user_temp$user_id)

    return(
        list(
            facility_selected = facility_selected,
            user_affected = user_affected,
            n_added = x$n_added
            # not really sure if I need to provide the user + facility solution
            # but perhaps I could provide this in another function to extract
            # the working parts of the optimisation
            # user_solution = user_solution,
            # facility_solution = facility_solution,
            # facilities_users_merge = facilities_users_merge,
            #add the variables that were used here to get more info
        )
    )

}

