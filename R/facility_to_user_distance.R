#' facility_to_user_distance
#'
#' uses haversines formula to calculate the distance between two sets of points. It calculates the distance from every X to every y. This came form an example of finding the distance from AEDs (here listed as X) to each OHCA (here listed as Y)
#'
#' @param df_facility_lat a vector from a data frame, x, containing latitude
#' @param df_facility_long a vector from a dataframe, x, containing longitude
#' @param df_user_lat a vector from a data frame, y, containing latitude
#' @param df_user_long a vector from a data frame, y, containing longitude
#'
#' @return a list where each element contains all of the distances from of every Y to every X.
#'
#' @export
#'
# @examples
#'
facility_to_user_distance <- function(df_facility_lat,
                                      df_facility_long,
                                      df_user_lat,
                                      df_user_long){


    dist_list <- vector("list", length(df_user_lat))

    for(i in 1:length(df_user_lat)){

        dist_list[[i]] <- spherical_distance(
            lat1 = df_facility_lat[1:length(df_facility_lat)], #aed
            long1 = df_facility_long[1:length(df_facility_long)], #aed
            lat2 = df_user_lat[i], # OHCA
            long2 = df_user_long[i]
            ) #OHCA
    } # end loop

    return(dist_list)

} # end function
