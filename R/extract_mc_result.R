#' extract_mc_results
#'
#' \code{extract_mc_results} takes a fitted max_coverage object and returns useful summary information from the model
#'
#' @description extract_mc_results exists so that the manipulation functions for the outcomes from the lp solver have another home - this makes it easier to maintain this pacakge, and heeds to this idea of having functions that are specialised. The name of this function is likely to change in the near future.
#'
#' @param x the fitted model from max_coverage
#'
#' @return a tibble containing multiple dataframes summarising the model
#'
#' @export

extract_mc_results <- function(x){

# extract results ---------------------------------------------------------

    # get the dimenions from the matrix
    J <- nrow(x$A)
    I <- ncol(x$A)

    # which AEDs are to be used
    facility_solution <- x$lp_solution$solution[1:I]

    # which facilities are selected?
    facility_temp <- tibble::tibble(
        aed_id = readr::parse_number(colnames(x$A)),
        aed_chosen = facility_solution) %>%
        dplyr::filter(aed_chosen == 1)

    facility_selected <- x$facility %>%
        dplyr::filter(aed_id %in% facility_temp$aed_id)

    # which OHCAs are affected
    user_solution <- x$lp_solution$solution[c(I+1):c(I+J)]

    user_temp <- tibble::tibble(
        ohca_id = x$ohca_id,
        ohca_chosen = user_solution) %>%
        dplyr::filter(ohca_chosen == 1)

    user_affected <- x$user %>%
        dplyr::filter(event_id %in% user_temp$ohca_id)


return(
    list(
        facility_solution = facility_solution,
        facility_selected = facility_selected,
        user_solution = user_solution,
        user_affected = user_affected
        # facilities_users_merge = facilities_users_merge,
        #add the variables that were used here to get more info
    )
)

}

