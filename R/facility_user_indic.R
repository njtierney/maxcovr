#' facility_user_indic
#'
#' This is a data manipulation function for facility_user_dist. This function creates a spread matrix of the distances between each ohca and each aed. There is an ohca_id column, and then a column for each aed_id, with a given cell being the distance between an ohca in a row, and that column. This distance is converted into an indicator variable, based upon whether that distance is less than the provided dist_indic parameter. In the future I might change the dist_indic function to be optional, but this whole function mainly exists to make it easier to do the computation in the max_coverage function.
#'
#' @param df_dist a dataframe from facility_user_dist. Requires nearest = "both"
#' @param dist_indic an indicator of the distance you want to be TRUE / FALSE
#'
#' @return a dataframe with variables ohca_id, and aed_id_number, with the id from each aed_id being transposed into each column name.
#' @export
#'
facility_user_indic <- function(df_dist,
                                dist_indic){

    # get the distance data frame back
    # df_dist <- facility_user_dist(facility = facility,
    #                               user = user,
    #                               coverage_distance = dist_indic,
    #                               nearest = "both")

    df_dist_indic_mat <- df_dist %>%
        dplyr::select(user_id,
                      facility_id,
                      distance) %>%
        # create the indicator variable - is the distance
        # less than the indicator? 100m is the default
        dplyr::mutate(distance_indic = (distance <= dist_indic)) %>%
        dplyr::select(-distance) %>%
        # spread this out so we can get this in a matrix format
        # so df[1,1] is the distance between AED#1 and OHCA#1
        tidyr::spread(key = "facility_id",
                      value = "distance_indic",
                      sep = "_") %>%
        # get rid of the ohca_id as this makes it hard to read later
        # select(-ohca_id) %>%
        as.matrix()

    return(df_dist_indic_mat)

}
