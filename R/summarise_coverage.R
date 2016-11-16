#' summarise_coverage
#'
#' Provides summary information of the coverage, using the distance dataframe created by maxcovr::facility_user_dist().
#'
#' @param df_dist distance matrix, as computed by facility_user_dist
#' @param dist_indic the critical distance range that you would like to know,
#'     default is 100m
#' @param spread do you want this in a one row summary, or a two row summary?
#'     Default is TRUE, a one row summary
#'
#' @return dataframe
#' @export
#'
summarise_coverage <- function(df_dist,
                               dist_indic = 100,
                               spread = TRUE){

    if(spread == FALSE){

        df_dist %>%
            dplyr::count(is_covered = distance < dist_indic)  %>%
            dplyr::mutate(is_covered = dplyr::if_else(is_covered == TRUE,
                                                      true = "Covered",
                                                      false = "Not Covered")) %>%
            dplyr::mutate(pct = n / sum(n)) %>%
            dplyr::rename(n_cov = n,
                          pct_cov = pct)
    } else if(spread == TRUE){

        df_dist %>%
            dplyr::summarise(distance_within = dist_indic,
                             n_cov = sum(is_covered),
                             pct_cov = (sum(is_covered) / nrow(.)),
                             n_not_cov =  (sum(is_covered == 0)),
                             pct_not_cov = (sum(is_covered == 0) / nrow(.)))
    }

}
