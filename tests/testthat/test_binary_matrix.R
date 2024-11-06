library(dplyr)
library(tibble)
library(tidyr)

facility_test_cpp <- york |>
    select(lat, long) |>
    slice(1:100) |>
    as.matrix()

user_test_cpp <- york_crime |>
    select(lat, long) |>
    slice(1:100) |>
    as.matrix()

my_bin_cpp <- binary_matrix_cpp(facility = facility_test_cpp,
                                user = user_test_cpp,
                                distance_cutoff = 100)

facility <- mutate(york, key = 1) |>
    rename(lat_facility = lat,
                  long_facility = long) |>
    # create an ID for each row
    mutate(facility_id = 1:n()) |>
    slice(1:100)

user <- mutate(york_crime, key = 1) |>
    rename(lat_user = lat,
                  long_user = long) |>
    mutate(user_id = 1:n()) |>
    slice(1:100)

my_bin_dplyr <- user |>
    left_join(facility,
              by = "key",
              relationship = "many-to-many") |>
    mutate(distance = spherical_distance(lat1 = lat_user,
                                                long1 = long_user,
                                                lat2 = lat_facility,
                                                long2 = long_facility)) |>
    # drop key
    select(-key) |>
    facility_user_indic(dist_indic = 100) |>
    # drop ID column
    _[,-1]

test_that("cpp binary matrix produces integer result as using dplyr method",{
    # I still need to make a method that gives the big matrix names
    expect_equal(my_bin_cpp,my_bin_dplyr, ignore_attr = TRUE)
})

