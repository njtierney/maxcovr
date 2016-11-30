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
#' mc_cv <- modelr::crossv_kfold(york_crime, 5) %>%
#'                  mutate(test = map(test,as_tibble),
#'                  train = map(train,as_tibble))
#'
#' mc_cv_fit <- map_df(mc_cv$train,
#'                     ~max_coverage(existing_facility = york_selected,
#'                     proposed_facility = york_unselected,
#'                     user = .,
#'                     n_added = 20,
#'                     distance_cutoff = 100))
#'
#'  summary_mc_cv(mc_cv_fit,
#'                mc_cv$test)
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
                                 dist_sd = sd(distance))

                   } # end internal function
) # close pmap

} # close function
