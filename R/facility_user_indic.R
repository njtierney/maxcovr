#' facility_user_indic
#'
#' This is a data manipulation function for facility_user_dist. This function
#'   creates a spread matrix of the distances between each user and each facility.
#'   There is an ohca_id column, and then a column for each aed_id, with a
#'   given cell being the distance between an ohca in a row, and that column.
#'   This distance is converted into an indicator variable, based upon whether
#'   that distance is less than the provided dist_indic parameter. In the
#'   future I might change the dist_indic function to be optional, but this
#'   whole function mainly exists to make it easier to do the computation in
#'   the max_coverage function.
#'
#' @param df_dist dataframe from facility_user_dist. Requires nearest = "both"
#' @param dist_indic an indicator of the distance you want to be TRUE / FALSE
#'
#' @return dataframe with variables user_id, and facility_id_number, with the id
#'   from each facility_id being transposed into each column name.
#' @export
#'
facility_user_indic <- function(df_dist,
                                dist_indic){

    # get the distance data frame back

    df_dist_indic_mat <- df_dist |>
        dplyr::select(user_id,
                      facility_id,
                      distance) |>
        # create indicator variable whether distance is less than indicator?
        # *>* 100m is the default *<*
        dplyr::mutate(distance_indic = as.integer((distance <= dist_indic))) |>
        dplyr::select(-distance) |>
        # spread this out so we can get this in a matrix format
        # so df[1,1] is the distance between AED#1 and OHCA#1
        tidyr::spread(key = "facility_id",
                      value = "distance_indic",
                      sep = "_") |>
        as.matrix()

    return(df_dist_indic_mat)

}
