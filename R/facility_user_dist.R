#' facility_user_dist
#'
#' Uses haversines formula to calculate the distance between lat/long co-ordinates of every facility and every user, returning a data_frame. You can think of "facilities" as something like mobile towers, police centres, or AED locations, and "users" as something like individual houses, crime locations, or heart attack locations. The motivating example for this function was finding the distance from Automatic Electronic Defibrillators (AEDs) to each Out of Hospital Cardiac Arrest (OHCA), where the locations for AEDs and OHCAs are in separate dataframes. Currently facifacility_user_dist makes the strict assumption that the facility and user dataframes have columns named aed_id, lat, and long, and ohca_id, lat, and long. This will be updated soon.
#'
#' @param facility a dataframe containing columns named "lat", and "long".
#' @param user a dataframe containing columns "lat", and "long".
#' @param coverage_distance numeric indicating the coverage level for the facilities to be within in metres to a user. Default value is 100 metres.
#' @param nearest character Can be "facility", "user", and "both". Defaults to "facility". When set to "facility", returns a dataframe where every row is every crime, and the closest building to each crime. When set to "user", returns a dataframe where every row is every building, and the closest crime to each building. set to "both", which will return every pairwise combination of distances. Be careful whenDefault is "facility"
#'
#' @return a data frame containing the two datasets joined together with columns named facility_id, lat_facility, long_facility, user_id, lat_user, long_user, distance in meters between each the given facility and user in a row.
#'
#' @export
#'
facility_user_dist <- function(facility,
                               user,
                               coverage_distance = 100,
                               nearest = "facility"){

    # check that lat and long are specified -----------------------------------
    # if(
    #     (c("lat") %in% names(facility) == TRUE) |
    #     (c("long") %in% names(facility) == TRUE) |
    #     (c("lat") %in% names(user) == TRUE) |
    #     (c("long") %in% names(user) == TRUE)
    #     ){
    #     warning("make sure lat and long are in the names")
    # }

    # dodgy method to get the cross product ---------------------------------------

    # do a dodgy cross product by adding a column of 1
    # and then joining on this column
    facility <- dplyr::mutate(facility, key = 1) %>%
        dplyr::rename(lat_facility = lat,
                      long_facility = long) %>%
        # create an ID for each row
        dplyr::mutate(facility_id = 1:n())

    user <- dplyr::mutate(user, key = 1) %>%
        dplyr::rename(lat_user = lat,
                      long_user = long) %>%
        dplyr::mutate(user_id = 1:n())

    dist_df <- user %>%
        dplyr::left_join(facility,
                         by = "key") %>%
        dplyr::mutate(distance = spherical_distance(lat1 = lat_user,
                                                    long1 = long_user,
                                                    lat2 = lat_facility,
                                                    long2 = long_facility)) %>%
        # drop key
        dplyr::select(-key)

    # calculate information about coverage for the OHCAs to AEDs.
    # switch here either :
    # finds the nearest AED to each OHCA
    # finds the nearest OHCA to each AED

    if (nearest == "facility"){

        dist_df <-
            dist_df %>%
            arrange(distance) %>%
            group_by(user_id) %>%
            # find those that are closest to each other
            mutate(rank_distance = 1:n()) %>%
            ungroup() %>%
            filter(rank_distance == 1) %>%
            # drop the rank_distance
            select(-rank_distance) %>%
            mutate(is_covered = (distance < coverage_distance))

        return(dist_df)

    } else if (nearest == "user"){

        dist_df <- dist_df %>%
            group_by(facility_id) %>%
            arrange(distance) %>%
            mutate(rank_distance = 1:n()) %>%
            ungroup() %>%
            filter(rank_distance == 1) %>%
            select(-rank_distance) %>%
            mutate(is_covered = (distance < coverage_distance))

        return(dist_df)

    } else if (nearest == "both"){

        return(dist_df)

    }

    # return a dataframe
    # return(dist_df)

    # option to spread?
    # select(ohca_id,
    #        aed_id,
    #        distance) %>%
    # spread(key = "aed_id",
    #        value = "distance",
    #        sep = "_")

}
