#' facility_user_dist
#'
#' This is a more sensible implementation of facility_to_user_dist.
#' Basically it returns a dataframe, not a mega-uber-list.
#' I need to work on what you need to pass the function.
#' Currently facility needs to have columns called aed_id, lat, and long, and user needs to have columns called ohca_id, lat, and long.
#'
#' @param facility a dataframe containing lat/long
#' @param user a dataframe containing
#'
#' @return a data frame containing the distance between each aed and each ohca
#' @export
#'
#' @examples
facility_user_dist <- function(facility,
                               user){

# dodgy method to get the cross product ---------------------------------------

    # do a dodgy cross product by adding a column of 1
    # and then joining on this column
    facility <- dplyr::mutate(facility, key = 1) %>%
        # downsize to
        dplyr::select(key,
               aed_id,
               lat,
               long) %>%
        dplyr::rename(lat_aed = lat,
               long_aed = long)

    user <- dplyr::mutate(user, key = 1) %>%
        dplyr::select(key,
               event_id,
               lat,
               long) %>%
        dplyr::rename(lat_ohca = lat,
               long_ohca = long,
               ohca_id = event_id)


    dist_df <- user %>%
        dplyr::left_join(facility,
                  by = "key") %>%
        dplyr::mutate(distance = spherical_distance(lat1 = lat_ohca,
                                                    long1 = long_ohca,
                                                    lat2 = lat_aed,
                                                    long2 = long_aed)) %>%
        # drop key
        dplyr::select(-key)

    # return a dataframe
    return(dist_df)
    # option to spread
    # select(ohca_id,
    #        aed_id,
    #        distance) %>%
    # spread(key = "aed_id",
    #        value = "distance",
    #        sep = "_")

}
