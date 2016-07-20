#' x_to_y_distance
#'
#' uses haversines formula to calculate the distance between two sets of points. It calculates the distance from every X to every y. This came form an example of finding the distance from AEDs (here listed as X) to each OHCA (here listed as Y)
#'
#' @param df_x_lat a vector from a data frame, x, containing latitude
#' @param df_x_long a vector from a dataframe, x, containing longitude
#' @param df_y_lat a vector from a data frame, y, containing latitude
#' @param df_y_long a vector from a data frame, y, containing longitude
#'
#' @return a list where each element contains all of the distances from of every Y to every X.
#'
#' @export
#'
# @examples
#'
x_to_y_distance <- function(df_x_lat,
                            df_x_long,
                            df_y_lat,
                            df_y_long){

    dist_list <- vector("list", length(df_y_lat))

    for(i in 1:length(df_y_lat)){

        dist_list[[i]] <- rnoaa::meteo_spherical_distance(
            lat1 = df_x_lat[1:length(df_x_lat)], #aed
            long1 = df_x_long[1:length(df_x_long)], #aed
            lat2 = df_y_lat[i], # OHCA
            long2 = df_y_long[i]
            ) #OHCA
    } # end loop

    return(dist_list)

} # end function
