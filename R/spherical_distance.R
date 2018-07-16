#' Calculate the distance between two locations
#'
#' This function uses the haversine formula to calculate the great circle
#' distance between two locations, identified by their latitudes and longitudes.
#' It is borrowed from rnoaa
#' (https://github.com/ropenscilabs/rnoaa/blob/master/R/meteo_distance.R) and
#' included here as rnoaa is a large package that is rather unrelated to
#' maxcovr. I have renamed it from meteo_spherical_distance to
#' spherical_distance
#'
#' @param lat1 Latitude of the first location.
#' @param long1 Longitude of the first location.
#' @param lat2 Latitude of the second location.
#' @param long2 Longitude of the second location.
#'
#' @return A numeric value giving the distance in meters between the
#'    pair of locations.
#'
#' @note This function assumes an earth radius of 6,371 km.
#'
#' @author Alex Simmons \email{a2.simmons@@qut.edu.au},
#'    Brooke Anderson \email{brooke.anderson@@colostate.edu}
#'
#' @examples
#'
#' spherical_distance(lat1 = -27.4667,
#'                    long1 = 153.0217,
#'                    lat2 = -27.4710,
#'                    long2 = 153.0234)
#'
#' @export
spherical_distance <- function(lat1,
                               long1,
                               lat2,
                               long2) {
    radius_earth <- 6371

    # Convert angle values into radians
    lat1 <- deg2rad(lat1)
    long1 <- deg2rad(long1)
    lat2 <- deg2rad(lat2)
    long2 <- deg2rad(long2)

    # Determine distance using haversines formula, assuming a spherical earth
    a <- sin((lat2 - lat1) / 2) ^ 2 + cos(lat1) * cos(lat2) *
        sin((long2 - long1) / 2) ^ 2

    d <- 2 * atan2(sqrt(a), sqrt(1 - a)) * radius_earth

    # return distance in kilometres
    d <- d * 1000
    # return(d)
    # return the distance in kilometers
    return(d)

} # End function

#' Convert from degrees to radians
#'
#' @param deg A numeric vector in units of degrees.
#'
#' @return The input numeric vector, converted to units of radians.
deg2rad <- function(deg) {
    return(deg * pi / 180)
} # End deg2rad

