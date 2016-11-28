library(tidyverse)
library(maxcovr)
context("equality of binary matrices")

my_bin_cpp <- binary_matrix_cpp(facility = facility_test_cpp,
                                user = user_test_cpp,
                                distance_cutoff = 100)


facility <- dplyr::mutate(york, key = 1) %>%
    dplyr::rename(lat_facility = lat,
                  long_facility = long) %>%
    # create an ID for each row
    dplyr::mutate(facility_id = 1:n())

user <- dplyr::mutate(york_crime, key = 1) %>%
    dplyr::rename(lat_user = lat,
                  long_user = long) %>%
    dplyr::mutate(user_id = 1:n())

my_bin_dplyr <- user %>%
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
    # create the indicator variable - is the distance
    # less than the indicator? 100m is the default
    dplyr::mutate(distance_indic = (distance <= 100)) %>%
    dplyr::select(-distance) %>%
    # spread this out so we can get this in a matrix format
    # so df[1,1] is the distance between AED#1 and OHCA#1
    tidyr::spread(key = "facility_id",
                  value = "distance_indic",
                  sep = "_") %>%
    # drop the ID column (for proper comparison)
    select(-user_id) %>%
    as.matrix()


testthat::test_that("cpp binary matrix produces the integer result as using dplyr method",{
    # I still need to make a method that gives the big matrix names
    testthat::expect_equal(my_bin_cpp,my_bin_dplyr, check.attributes = FALSE)
})

# my_bin_cpp[1203,2031]
# my_bin_dplyr[1203,2031]
