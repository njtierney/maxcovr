#' extract_mc_results_relocation
#'
#' \code{extract_mc_results_relocation} takes a fitted max_coverage object and returns useful summary information from the model, specifically for the relocation method.
#'
#' @description extract_mc_results exists so that the manipulation functions for the outcomes from the lp solver have another home - this makes it easier to maintain this package, and heeds to this idea of having functions that are specialised. The name of this function is likely to change in the near future.
#'
#' @param x the fitted model from max_coverage
#'
#' @return a list containing multiple dataframes summarising the model
#'
#' @examples

#'
#' library(dplyr)
#' #'
#' # subset to be the places with towers built on them.
#'
#' york_selected <- york %>% filter(grade == "I")
#'
#' york_unselected <- york %>% filter(grade != "I")
#'
#' # OK, what if I just use some really crazy small data to optimise over.
#'
#' #
#'
#' mc_relocate <-  max_coverage_relocation(existing_facility = york_selected,
#'                                         proposed_facility = york_unselected,
#'                                         user = york_crime,
#'                                         distance_cutoff = 100,
#'                                         cost_install = 5000,
#'                                         cost_relocate = 200,
#'                                         cost_total = 600000,
#'                                         # cost_total = 515400,
#'                                         # cost_total = 10^6,
#'                                         # cost_total = 10000,
#'                                         # n_added = nrow(existing_facility),
#'                                         n_solutions = 1)

# extract results ---------------------------------------------------------

extract_mc_results_relocation <- function(x){

# using results extract_mc_result
# in the future this will give the output of max_coverage a class
# and then extract_mc_result will be an S3 method
# where the output in this case of relocation will be something like
# "mc_relocation"
# as opposed to the other one, which might be "mc_not_relocate", or something.

x <- mc_relocate
# get the dimenions from the matrix
J <- nrow(x$A)
I <- ncol(x$A)


# how many are in existence?
n_existing <- nrow(x$existing_facility)

# how many are proposed?
n_proposed <- nrow(x$proposed_facility)

# x$lp_solution$solution[1:n_existing]

# how many were selected?
n_existing_selected <- sum(x$lp_solution$solution[1:n_existing])

# how many from the existing locations were moved?
n_existing_removed <- n_existing - n_existing_selected

# which ones were moved?
which_existing_removed <-
    x$existing_facility[which(x$lp_solution$solution[1:n_existing] == 0),]

# how many additional proposed ones were selected?
n_bit_1 <- n_existing+1
n_bit_2 <- n_existing+n_proposed

n_proposed_chosen <- sum(x$lp_solution$solution[n_bit_1:n_bit_2])

# OK, so the good news is that 103 were still moved...

# maybe I need to specify another $400 to get the other two moved/
# and oddly, that worked exactly...
# maybe there is now total coverage?

n_bit_3 <- n_existing+n_proposed+1

n_bit_4 <- length(x$lp_solution$solution)

# number of users affected
n_users_affected <- sum(x$lp_solution$solution[n_bit_3:n_bit_4])

# 693 things are covered.
# which ones?
# and how do I assess this? - it's been assesed later in the code

# key pieces of information
# n_proposed_chosen
# n_existing_removed
# which_existing_removed

# back to old summary method ---------------------------------------------------

# ... surprisingly (!) this works with very few changes  good job, past nick!

# which AEDs are to be used
facility_solution <- x$lp_solution$solution[1:I]

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
user_solution <- x$lp_solution$solution[c(I+1):c(I+J)]

user_temp <- tibble::tibble(
    user_id = x$user_id,
    user_chosen = user_solution) %>%
    dplyr::filter(user_chosen == 1)

user_affected <- dplyr::left_join(user_temp,
                                  # x$user_not_covered,
                                  x$existing_user,
                                  by = "user_id")

# x$user %>%
# mutate(user_id = user_id) %>%
# dplyr::filter(user_id %in% user_temp$user_id) %>%
# # drop user_id, as it not needed anymore
# select(-user_id)
# dplyr::filter(event_id %in% user_temp$user_id)

# now to return some more summaries ...

# NOTE: I really should use `nearest`
facility_sum_prep <- dplyr::bind_rows(facility_selected,
                                      x$existing_facility) %>%
    dplyr::select(lat,long) %>%
    as.matrix()

user_sum_prep <- x$existing_user %>%
    dplyr::select(lat,long) %>%
    as.matrix()

dist_sum_df <- maxcovr::nearest_facility_dist(facility = facility_sum_prep,
                                              user = user_sum_prep) %>%
    dplyr::as_data_frame() %>%
    dplyr::rename(user_id = V1,
                  facility_id = V2,
                  distance = V3) %>%
    dplyr::mutate(is_covered = (distance <= x$distance_cutoff))

model_coverage <- dist_sum_df %>%
    dplyr::summarise(
        total_cost = as.numeric(x$cost_total),
        install_cost = as.numeric(x$cost_install),
        cost_relocate = as.numeric(x$cost_relocate),
        n_proposed_chosen = n_proposed_chosen,
        n_existing_removed = n_existing_removed,
        n_added = as.numeric(x$n_added),
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
    dplyr::summarise(n_added = 0,
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


return(
    tibble::tibble(
        facility_selected = list(facility_selected),
        user_affected = list(user_affected),
        which_existing_removed = list(which_existing_removed),
        model_coverage = list(model_coverage),
        existing_coverage = list(existing_coverage),
        summary = list(summary_coverage),
        total_cost = list(x$cost_total),
        distance_cutoff = list(x$distance_cutoff)
        # not really sure if I need to provide the user + facility solution
        # but perhaps I could provide this in another function to extract
        # the working parts of the optimisation
        # user_solution = user_solution,
        # facility_solution = facility_solution,
        # facilities_users_merge = facilities_users_merge,
        #add the variables that were used here to get more info
    )
)

} # end the function
