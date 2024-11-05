library(dplyr)
library(tibble)
library(tidyr)

facility_test_cpp <- york %>%
    select(lat, long) %>%
    slice(1:100) %>%
    as.matrix()

user_test_cpp <- york_crime %>%
    select(lat, long) %>%
    slice(1:100) %>%
    as.matrix()

my_bin_cpp <- binary_matrix_cpp(facility = facility_test_cpp,
                                user = user_test_cpp,
                                distance_cutoff = 100)

facility <- mutate(york, key = 1) %>%
    rename(lat_facility = lat,
                  long_facility = long) %>%
    # create an ID for each row
    mutate(facility_id = 1:n()) %>%
    slice(1:100)

user <- mutate(york_crime, key = 1) %>%
    rename(lat_user = lat,
                  long_user = long) %>%
    mutate(user_id = 1:n()) %>%
    slice(1:100)

my_bin_dplyr <- user %>%
    left_join(facility,
                     by = "key") %>%
    mutate(distance = spherical_distance(lat1 = lat_user,
                                                long1 = long_user,
                                                lat2 = lat_facility,
                                                long2 = long_facility)) %>%
    # drop key
    select(-key) %>%
    select(user_id,
                  facility_id,
                  distance) %>%
    # create the indicator variable - is the distance
    # less than the indicator? 100m is the default
    mutate(distance_indic = (distance <= 100)) %>%
    select(-distance) %>%
    # spread this out so we can get this in a matrix format
    # so df[1,1] is the distance between AED#1 and OHCA#1
    spread(key = "facility_id",
                  value = "distance_indic",
                  sep = "_") %>%
    # drop the ID column (for proper comparison)
    select(-user_id) %>%
    as.matrix()


test_that("cpp binary matrix produces  integer result as using dplyr method",{
    # I still need to make a method that gives the big matrix names
    expect_equal(my_bin_cpp,my_bin_dplyr, check.attributes = FALSE)
})

