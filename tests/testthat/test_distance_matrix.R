library(maxcovr)
library(dplyr)
library(tibble)
library(tidyr)

context("missings in distance_matrix")

facility_test_cpp <- york %>%
    select(lat, long) %>%
    slice(1:100) %>%
    as.matrix()

user_test_cpp <- york_crime %>%
    select(lat, long) %>%
    slice(1:100) %>%
    as.matrix()

my_dist_cpp <- distance_matrix_cpp(facility_test_cpp, user_test_cpp)

testthat::test_that("There are no missing values in distance_matrix",{
    testthat::expect_equal(sum(is.na(my_dist_cpp)), 0)
})

context("equality of distance matrices")

facility_test_cpp <- york %>%
    select(lat, long) %>%
    slice(1:100) %>%
    as.matrix()

user_test_cpp <- york_crime %>%
    select(lat, long) %>%
    slice(1:100) %>%
    as.matrix()

my_dist_cpp <- distance_matrix_cpp(facility_test_cpp, user_test_cpp)

# compare this to the dplyr method ============================================

facility <- dplyr::mutate(york, key = 1) %>%
    dplyr::rename(lat_facility = lat,
                  long_facility = long) %>%
    # create an ID for each row
    dplyr::mutate(facility_id = 1:dplyr::n()) %>%
    slice(1:100)

user <- dplyr::mutate(york_crime, key = 1) %>%
    dplyr::rename(lat_user = lat,
                  long_user = long) %>%
    dplyr::mutate(user_id = 1:dplyr::n()) %>%
    slice(1:100)

my_dist_dplyr <- user %>%
    dplyr::left_join(facility,
                     by = "key") %>%
    dplyr::mutate(distance = spherical_distance(lat1 = lat_user,
                                                long1 = long_user,
                                                lat2 = lat_facility,
                                                long2 = long_facility)) %>%
    # drop key
    dplyr::select(-key) %>%
    dplyr::select(user_id,
                  facility_id,
                  distance) %>%
    tidyr::spread(key = "facility_id",
                  value = "distance",
                  sep = "_") %>%
    # drop the ID column (for proper comparison)
    select(-user_id) %>%
    as.matrix()


testthat::test_that("cpp distance matrix produces same numeric result as using dplyr method",{
    # I still need to make a method that gives the big matrix names
    testthat::expect_equal(my_dist_cpp,my_dist_dplyr, check.attributes = FALSE)
})
