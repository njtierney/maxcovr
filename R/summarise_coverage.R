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
                         dist_sd = sd(distance))

}
