#' facility_user_indic
#'
#' This is a data manipulation function for facility_user_dist. This function creates a spread matrix of the distances between each ohca and each aed. There is an ohca_id column, and then a column for each aed_id, with a given cell being the distance between an ohca in a row, and that column. This distance is converted into an indicator variable, based upon whether that distance is less than the provided dist_indic parameter. In the future I might change the dist_indic function to be optional, but this whole function mainly exists to make it easier to do the computation in the max_coverage function.
#'
#' @param facility a dataframe containing columns aed_id, lat, long
#' @param user a dataframe containing columns ohca_id, lat, long
#' @param dist_indic the distance in meters that you are interested in
#'
#' @return a dataframe with variables ohca_id, and aed_id_number, with the id from each aed_id being transposed into each column name.
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
