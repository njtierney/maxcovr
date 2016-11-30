#' Find the nearest lat/long to another lat/long
#'
#' This function finds the nearest lat/long pairs to another lat/long pair.
#' So in the york building and york crime context, writing
#' \code{nearest(york_crime,york)} reads as "find the nearest crime in york to
#' each building in york, and returns a dataframe with every building in york,
#' the nearest york_crime to each building, and the distance in metres between
#' the two. Likewise, you could write \code{nearest(york, york_crime)}, and this
#' would return the nearest building to every crime. \code{nearest} assumes that
#' the names of the latitude and longitude are "lat" and "long", but you can
#' provide these names.
#'
#' @param nearest_df a dataframe containing latitude and longitude
#' @param to_df a dataframe containing latitude and longitude
#' @param nearest_lat name of latitude in nearest_df
#' @param nearest_long name of longitude in nearest_df
#' @param to_lat name of latitude in to_df
#' @param to_long name of longitude in to_df
#'
#' @return dataframe of "to_df" along with the nearest "nearest_df" to each row,
#' along with the distance between the two, and the nearest_id, the row position
#' of the nearest_df closest to that row.
#'
#' @examples
#'
#' library(maxcovr)
#'
#' nearest(nearest_df = york_crime,
#'         to_df = york)
#'
#' # you can use the pipe as well
#'
#' \dontrun{
#'
#' library(magrittr)
#' york_crime %>% nearest(york)
#'
#' }
#'
#' @export
nearest <- function(nearest_df,
                    to_df,
                    nearest_lat = "lat",
                    nearest_long = "long",
                    to_lat = "lat",
                    to_long = "long"){

    # this function is syntactic sugar to find the nearest lat/long from "nearest"
    # "to" another lat/long.
    # it reads quite nicely:
    # facility %>% nearest(user)
    # user %>% nearest(facility)
    # nearest(nearest_df = facility, to_df = user)
    # nearest(nearest_df = user, to_df = facility)

    nearest_mat <- as.matrix(nearest_df[c(nearest_lat,nearest_long)])
    to_mat <- as.matrix(to_df[c(to_lat,to_long)])

    dist_mat <- nearest_facility_dist(facility = nearest_mat, # aed or building
                                      user = to_mat) # ohca or crime

    # repeat after me: "nearest(facility) to user"
    # nearest = facility; to = user

    dist_df <- dist_mat %>%
        tibble::as_tibble() %>%
        dplyr::rename(to_id = V1,
                      nearest_id = V2,
                      distance = V3)

    # there will need to be an option to add your own special ID
    # because we are sorta hard coding the IDs, this would all break apart if
    # the rows were differently arranged.

    # create some IDs to join by
    to_df_id <- to_df %>% dplyr::mutate(to_id = 1:n())
    nearest_df_id <- nearest_df %>% dplyr::mutate(nearest_id = 1:n())

    # dat_user_facility_dist <- dist_df %>%
    nearest_to_dist_df <- dist_df %>%
        dplyr::left_join(to_df_id,
                         by = "to_id") %>%
        dplyr::left_join(nearest_df_id,
                         by = "nearest_id") %>%
        dplyr::rename(long_to = long.x,
                      lat_to = lat.x,
                      long_nearest = long.y,
                      lat_nearest = lat.y)

    # consider
    # unsure if I want to join on the OHCA (user) data to this?
    # this line would follow the above
    # nearest_df_id <- nearest_df %>% dplyr::mutate(nearest_id = 1:n())
    # left_join(dat_user_id,
    #           by = "user_id")
    # but perhaps if people really want to do this they just do it themselves?

    return(nearest_to_dist_df)

    # do I need to join on the facility information as well?
    # option to add your own special ID
    # document that the order of the rows is really important.
    # otherwise, provide the IDs

    # timing is 0.268 seconds compared to 10 seconds with the old method
    # this also has the nice feature of being reversible

}
