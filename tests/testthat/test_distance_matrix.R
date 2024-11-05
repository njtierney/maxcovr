library(dplyr)
library(tibble)
library(tidyr)

# missings in distance_matrix

facility_test_cpp <- york |>
    select(lat, long) |>
    slice(1:100) |>
    as.matrix()

user_test_cpp <- york_crime |>
    select(lat, long) |>
    slice(1:100) |>
    as.matrix()

my_dist_cpp <- distance_matrix_cpp(facility_test_cpp, user_test_cpp)

test_that("There are no missing values in distance_matrix",{
    expect_equal(sum(is.na(my_dist_cpp)), 0)
})

# equality of distance matrices

facility_test_cpp <- york |>
    select(lat, long) |>
    slice(1:100) |>
    as.matrix()

user_test_cpp <- york_crime |>
    select(lat, long) |>
    slice(1:100) |>
    as.matrix()

my_dist_cpp <- distance_matrix_cpp(facility_test_cpp, user_test_cpp)

# compare this to the dplyr method ============================================

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

my_dist_dplyr <- user |>
    left_join(facility,
                     by = "key",
              relationship = "many-to-many") |>
    mutate(distance = spherical_distance(lat1 = lat_user,
                                                long1 = long_user,
                                                lat2 = lat_facility,
                                                long2 = long_facility)) |>
    # drop key
    select(-key) |>
    select(user_id,
                  facility_id,
                  distance) |>
    spread(key = "facility_id",
                  value = "distance",
                  sep = "_") |>
    # drop the ID column (for proper comparison)
    select(-user_id) |>
    as.matrix()


test_that("cpp distance matrix produces same numeric result as using dplyr",{
    # I still need to make a method that gives the big matrix names
    expect_equal(my_dist_cpp,my_dist_dplyr)
})
