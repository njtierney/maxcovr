#' Find the nearest lat/long to another lat/long
#'
#' This function finds the nearest lat/long pairs to another lat/long pair.
#'   So in the york building and york crime context, writing
#'   `nearest(york_crime,york)` reads as "find the nearest crime in york to
#'   each building in york, and returns a dataframe with every building in york,
#'   the nearest york_crime to each building, and the distance in metres between
#'   the two. Likewise, you could write `nearest(york, york_crime)`, and this
#'   would return the nearest building to every crime. `nearest` assumes that
#'   the names of the latitude and longitude are "lat" and "long", but you can
#'   provide these names.
#'
#' @param nearest_df a dataframe containing latitude and longitude.
#' @param to_df a dataframe containing latitude and longitude.
#' @param nearest_lat name of latitude in nearest_df.
#' @param nearest_long name of longitude in nearest_df.
#' @param to_lat name of latitude in to_df.
#' @param to_long name of longitude in to_df.
#'
#' @return dataframe of "to_df" along with the nearest "nearest_df" to each row,
#'   along with the distance between the two, and the nearest_id, the row
#'   position of the nearest_df closest to that row.
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
#' york_crime |> nearest(york)
#'
#' @export
nearest <- function(nearest_df,
                    to_df,
                    nearest_lat = "lat",
                    nearest_long = "long",
                    to_lat = "lat",
                    to_long = "long"){

    # this function is syntactic sugar to find nearest lat/long from "nearest"
    # "to" another lat/long.
    # it reads quite nicely

    nearest_mat <- as.matrix(nearest_df[c(nearest_lat,nearest_long)])
    to_mat <- as.matrix(to_df[c(to_lat,to_long)])

    dist_mat <- nearest_facility_dist(facility = nearest_mat,
                                      user = to_mat)

    dist_df <- dist_mat |>
        tibble::as_tibble(.name_repair = "unique_quiet") |>
        dplyr::rename(to_id = `...1`,
                      nearest_id = `...2`,
                      distance = `...3`)

    # there will need to be an option to add your own special ID
    # because we are sorta hard coding the IDs
    # this would all break apart if the rows were differently arranged.

    # create some IDs to join by
    to_df_id <-  dplyr::mutate(to_df, to_id = 1:dplyr::n())
    nearest_df_id <- dplyr::mutate(nearest_df, nearest_id = 1:dplyr::n())

    nearest_to_dist_df <- dist_df |>
        dplyr::left_join(to_df_id,
                         by = "to_id") |>
        dplyr::left_join(nearest_df_id,
                         by = "nearest_id") |>
        dplyr::rename(long_to = long.x,
                      lat_to = lat.x,
                      long_nearest = long.y,
                      lat_nearest = lat.y)

    return(nearest_to_dist_df)

}
