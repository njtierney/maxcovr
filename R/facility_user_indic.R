#' facility_user_indic
#'
#' Return a spread distance matrix
#'
#' @param facility a dataframe containing columns aed_id, lat, long
#' @param user a dataframe containing columns ohca_id, lat, long
#' @param dist_indic the distance in km that you are interested in
#'
#' @return a matrix with distance
#' @export
#'
facility_user_indic <- function(facility,
                                user,
                                dist_indic){

    # get the distance data frame back
    df_dist <- facility_user_dist(facility, user)

    df_dist_indic_mat <- df_dist %>%
        dplyr::select(ohca_id,
               aed_id,
               distance) %>%
        # create the indicator variable - is the distance
        # less than the indicator? 100m is the default
        dplyr::mutate(distance_indic = (distance <= dist_indic)) %>%
        dplyr::select(-distance) %>%
        # spread this out so we can get this in a matrix format
        # so df[1,1] is the distance between AED#1 and OHCA#1
        tidyr::spread(key = "aed_id",
                      value = "distance_indic",
                      sep = "_") %>%
        # get rid of the ohca_id as this makes it hard to read later
        # select(-ohca_id) %>%
        as.matrix()

    return(df_dist_indic_mat)

}
