#' facility_user_indic
#'
#' Return a spread distance matrix
#'
#' @param facility a dataframe containing columns aed_id, lat, long
#' @param user a dataframe containing columns ohca_id, lat, long
#' @param indic the distance in km that you are interested in
#'
#' @return a dataframe with a column of ohca IDs
#' @export
#'
#' @examples
facility_user_indic <- function(facility,
                                user,
                                indic = 0.1){

    # get the distance data frame back
    df_dist <- facility_user_dist(facility, user)

    df_dist_indic_mat <- df_dist %>%
        dplyr::select(ohca_id,
               aed_id,
               distance) %>%
        # create the indicator variable - is the distance
        # less than the indicator? 100m is the default
        dplyr::mutate(distance_indic = (distance <= indic)) %>%
        dplyr::select(-distance) %>%
        # spread this out so we can get this in a matrix format
        # so df[1,1] is the distance between AED#1 and OHCA#1
        tidyr::spread(key = "aed_id",
                      value = "distance_indic",
                      sep = "_")

    return(df_dist_indic_mat)

}
