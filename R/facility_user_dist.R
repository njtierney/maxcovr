#' facility_user_dist
#'
#' Uses haversines formula to calculate the distance between lat/long co-ordinates of every facility and every user, returning a data_frame. You can think of "facilities" as something like mobile towers, police centres, or AED locations, and "users" as something like individual houses, crime locations, or heart attack locations. The motivating example for this function was finding the distance from Automatic Electronic Defibrillators (AEDs) to each Out of Hospital Cardiac Arrest (OHCA), where the locations for AEDs and OHCAs are in separate dataframes. Currently facifacility_user_dist makes the strict assumption that the facility and user dataframes have columns named aed_id, lat, and long, and ohca_id, lat, and long. This will be updated soon.
#'
#' @param facility a dataframe containing columns aed_id, lat, and long
#' @param user a dataframe containing columns ohca_id, lat, and long
#'
#' @return a data frame containing the distance between each aed and each ohca, with columns named aed_id, lat_aed, long_aed, ohca_id, lat_ohca, long_ohca, and distance - the distance in meters between each aed and ohca in a row.
#'
#' @export
#'
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

    # option to spread?
        # select(ohca_id,
        #        aed_id,
        #        distance) %>%
        # spread(key = "aed_id",
        #        value = "distance",
        #        sep = "_")

}
