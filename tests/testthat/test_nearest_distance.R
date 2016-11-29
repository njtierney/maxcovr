library(tidyverse)
library(maxcovr)

context("equality of nearest distances")

facility_test_cpp <- york %>% select(lat, long) %>% as.matrix()
user_test_cpp <- york_crime %>% select(lat, long) %>% as.matrix()

near_cpp <- nearest_facility_dist(facility = facility_test_cpp,
                                  user = user_test_cpp)

# near_dplyr <- facility_user_dist(facility = york,
#                                  user = york_crime,
#                                  coverage_distance = 100)


# begin old dplyr way ---------------------------------------------------------

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
    arrange(distance) %>%
    group_by(user_id) %>%
    # find those that are closest to each other
    mutate(rank_distance = 1:n()) %>%
    ungroup() %>%
    filter(rank_distance == 1) %>%
    # drop the rank_distance
    select(-rank_distance)

near_dplyr_test <- near_dplyr %>%
    arrange(user_id,
            facility_id) %>%
    select(distance) %>%
    as.matrix()
    # select(user_id,
    #        facility_id,
    #        distance)

near_cpp_test <- near_cpp %>%
    as_tibble() %>%
    rename(user_id = V1,
           facility_id = V2,
           distance = V3) %>%
    arrange(user_id,
            facility_id) %>%
    select(distance) %>%
    as.matrix()

testthat::test_that("Nearest distances in cpp are same as dplyr method",{
    testthat::expect_equal(near_dplyr_test,near_cpp_test, check.attributes = FALSE)
    })
