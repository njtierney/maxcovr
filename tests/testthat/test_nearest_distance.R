context("equality of nearest distances")

library(dplyr)
library(tibble)
library(tidyr)
library(maxcovr)

facility_test_cpp <- york %>% select(lat, long) %>% as.matrix()
user_test_cpp <- york_crime %>% select(lat, long) %>% as.matrix()

near_cpp <- nearest_facility_dist(facility = facility_test_cpp,
                                  user = user_test_cpp)

# old dplyr way ===============================================================

facility <- dplyr::mutate(york, key = 1) %>%
    dplyr::rename(lat_facility = lat,
                  long_facility = long) %>%
    # create an ID for each row
    dplyr::mutate(facility_id = 1:n())

user <- dplyr::mutate(york_crime, key = 1) %>%
    dplyr::rename(lat_user = lat,
                  long_user = long) %>%
    dplyr::mutate(user_id = 1:n())

near_dplyr <- user %>%
    dplyr::left_join(facility,
                     by = "key") %>%
    dplyr::mutate(distance = spherical_distance(lat1 = lat_user,
                                                long1 = long_user,
                                                lat2 = lat_facility,
                                                long2 = long_facility)) %>%
    # drop key
    dplyr::select(-key) %>%
    dplyr::arrange(distance) %>%
    dplyr::group_by(user_id) %>%
    # find those that are closest to each other
    dplyr::mutate(rank_distance = 1:n()) %>%
    dplyr::ungroup() %>%
    dplyr::filter(rank_distance == 1) %>%
    # drop the rank_distance
    dplyr::select(-rank_distance)

near_dplyr_test <- near_dplyr %>%
    dplyr::arrange(user_id,
            facility_id) %>%
    dplyr::select(distance) %>%
    as.matrix()

near_cpp_test <- near_cpp %>%
    tibble::as_tibble() %>%
    dplyr::rename(user_id = V1,
           facility_id = V2,
           distance = V3) %>%
    dplyr::arrange(user_id,
            facility_id) %>%
    dplyr::select(distance) %>%
    as.matrix()

testthat::test_that("Nearest distances in cpp are same as dplyr method",{
    testthat::expect_equal(near_dplyr_test,near_cpp_test, check.attributes = FALSE)
    })
