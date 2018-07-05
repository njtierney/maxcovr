#' summarise_coverage
#'
#' Provides summary information of the coverage, using the distance dataframe created by maxcovr::facility_user_dist().
#'
#' @param df_dist distance matrix, as computed by facility_user_dist
#' @param dist_indic the critical distance range that you would like to know,
#'     default is 100m
#'
#' @return dataframe
#' @export
#'
summarise_coverage <- function(df_dist,
                               dist_indic = 100){

    df_dist %>%
        # allow for a programmatic way to specify a column named "distance"
        # mutate(is_covered = distance <= dist_indic) %>%
        dplyr::summarise(distance_within = dist_indic,
                         n_cov = sum(is_covered),
                         n_not_cov =  sum(is_covered == 0),
                         # divide by the number of total events covered, not the
                         # number of rows in the dataframe passed
                         # this allows it to behave with group_by in a more
                         # sensible and predictable way
                         pct_cov = sum(is_covered) / sum(n_cov, n_not_cov),
                         pct_not_cov = sum(is_covered == 0) / sum(n_cov,n_not_cov),
                         dist_avg = mean(distance),
                         dist_sd = stats::sd(distance))

}

#' Summary for max_coverage cross validation
#'
#' @param model the cross validated model
#' @param test_data the cross validated test data
#'
#' @return a summary dataframe
#'
#'
#' @examples
#'
#' \dontrun{
#'
#' library(maxcovr)
#' library(tidyverse)
#'
#' york_selected <- york %>% filter(grade == "I")
#' york_unselected <- york %>% filter(grade != "I")
#'
#' mc_cv_fixed <- modelr::crossv_kfold(york_crime, 5) %>%
#'                  mutate(test = map(test,as_tibble),
#'                  train = map(train,as_tibble))
#'
#' mc_cv_fit <- map_df(mc_cv_fixed$train,
#'                     ~max_coverage(existing_facility = york_selected,
#'                     proposed_facility = york_unselected,
#'                     user = .,
#'                     n_added = 20,
#'                     distance_cutoff = 100))
#'
#'  summary_mc_cv(mc_cv_fit,
#'                mc_cv_fixed$test)
#'
#' }
#'
#' @export
#'
#'
summary_mc_cv <- function(model,
                          test_data){

    purrr::pmap_df(.l = list(
        facility_selected = model$facility_selected,
        test_data = test_data$test,
        dist_cutoff = model$distance_cutoff,
        n_added = model$n_added,
        n_fold = test_data$.id
        ),

        .f = function(facility_selected, # the facility selected by max_coverage
                      test_data, # the test data created by modelr
                      dist_cutoff, # the distance cutoff
                      n_added, # the number of AEDs added
                      n_fold){
            maxcovr::nearest(nearest_df = facility_selected,
                             to_df = test_data) %>%
                dplyr::mutate(is_covered = (distance <= dist_cutoff)) %>%
                dplyr::summarise(n_added = n_added,
                                 n_fold = n_fold,
                                 distance_within = dist_cutoff,
                                 n_cov = sum(is_covered),
                                 pct_cov = (sum(is_covered) / nrow(.)),
                                 n_not_cov =  (sum(is_covered == 0)),
                                 pct_not_cov = (sum(is_covered == 0) / nrow(.)),
                                 dist_avg = mean(distance),
                                 dist_sd = stats::sd(distance))

                   } # end internal function
) # close pmap

} # close function

#' Create a summary of the coverage between two dataframes
#'
#' In the york building and york crime context, writing `nearest(york_crime,york)` reads as "find the nearest crime in york to each building in york, and returns a dataframe with every building in york, the nearest york_crime to each building, and the distance in metres between the two.
#'
#' @param nearest_df dataframe containing latitude and longitude
#' @param to_df dataframe containing latitude and longitude
#' @param distance_cutoff integer the distance threshold you are interested in assessing coverage at
#' @param ... extra arguments to pass to nearest
#'
#' @return a dataframe containing information about the distance threshold uses (distance_within), the number of events covered and not covered (n_cov, n_not_cov), the percentage covered and not covered (pct_cov, pct_not_cov), and the average distance and sd distance.
#'
#' @export
#'
#' @examples
#'
#' library(dplyr)
#'
#' # already existing locations
#' york_selected <- york %>% filter(grade == "I")
#'
#' # proposed locations
#' york_unselected <- york %>% filter(grade != "I")
#' coverage(york_selected, york_crime)
#' coverage(york_crime, york_selected)
#'
#'
coverage <- function(nearest_df,
                     to_df,
                     distance_cutoff = 100,
                     ...){

    nearest_df %>%
        # not sure if this is the best way to use ..., but it seems to work!
        # basically I want people to be able to pass arguments to nearest
        nearest(to_df, ...) %>%
        dplyr::mutate(is_covered = distance <= distance_cutoff) %>%
        summarise_coverage()

}
#' Summary for max_coverage cross validation for relocation models
#'
#' @param model the cross validated model
#' @param test_data the cross validated test data
#'
#' @return a summary dataframe
#'
#'
#' @examples
#'
#' \dontrun{
#'
#' library(maxcovr)
#' library(tidyverse)
#'
#' york_selected <- york %>% filter(grade == "I")
#' york_unselected <- york %>% filter(grade != "I")
#'
#' mc_cv <- modelr::crossv_kfold(york_crime, 5) %>%
#'                  mutate(test = map(test,as_tibble),
#'                  train = map(train,as_tibble))
#'
#' mc_cv_relocate <- map_df(mc_cv$train,
#'                     ~max_coverage_relocation(existing_facility = york_selected,
#'                                proposed_facility = york_unselected,
#'                                user = .,
#'                                cost_install = 2500,
#'                                cost_removal = 700,
#'                                cost_total = 50000,
#'                                distance_cutoff = 100,
#'                                solver = "gurobi"))
#' #'
#'  summary_mc_cv_relocate(mc_cv_relocate, mc_cv$test)
#'
#' }
#'
#' @export
#'
#'
summary_mc_cv_relocate <- function(model,
                                   test_data){

        # model = mc_cv_relocate
        # test_data = mc_cv$test
        cost = model$total_cost[[1]]
        purrr::pmap_df(.l = list(
            facility_selected = model$facility_selected,
            test_data = test_data$test,
            dist_cutoff = model$distance_cutoff,
            # cost = model$total_cost,
            n_fold = test_data$.id
        ),

        .f = function(facility_selected, # the facility selected by max_coverage
                      test_data, # the test data created by modelr
                      dist_cutoff, # the distance cutoff
                      n_added, # the number of AEDs added
                      n_fold){
            maxcovr::nearest(nearest_df = facility_selected,
                             to_df = test_data) %>%
                dplyr::mutate(is_covered = (distance <= dist_cutoff)) %>%
                dplyr::summarise(
                    # cost = cost,
                    n_fold = n_fold,
                    distance_within = dist_cutoff,
                    n_cov = sum(is_covered),
                    pct_cov = (sum(is_covered) / nrow(.)),
                    n_not_cov =  (sum(is_covered == 0)),
                    pct_not_cov = (sum(is_covered == 0) / nrow(.)),
                    dist_avg = mean(distance),
                    dist_sd = stats::sd(distance))

        } # end internal function
        ) %>%
            dplyr::mutate(cost = cost)

    } # close function
