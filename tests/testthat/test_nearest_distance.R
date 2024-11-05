library(dplyr)
library(tibble)
library(tidyr)

set.seed(2019-11-10)
facility_test_cpp <- york |>
    select(lat, long) |>
    slice(1:100) |>
    as.matrix()

user_test_cpp <- york_crime |>
    select(lat, long) |>
    slice(1:100) |>
    as.matrix()

near_cpp <- nearest_facility_dist(facility = facility_test_cpp,
                                  user = user_test_cpp)

# old dplyr way ===============================================================

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

near_dplyr <- user |>
    left_join(facility,
                     by = "key",
              relationship = "many-to-many") |>
    mutate(distance = spherical_distance(lat1 = lat_user,
                                                long1 = long_user,
                                                lat2 = lat_facility,
                                                long2 = long_facility)) |>
    # drop key
    select(-key) |>
    arrange(distance) |>
    group_by(user_id) |>
    # find those that are closest to each other
    mutate(rank_distance = 1:n()) |>
    ungroup() |>
    filter(rank_distance == 1) |>
    # drop the rank_distance
    select(-rank_distance)

near_dplyr_test <- near_dplyr |>
    arrange(user_id,
            facility_id) |>
    select(distance) |>
    as.matrix()

near_cpp_test <- near_cpp |>
    as_tibble(.name_repair = "unique_quiet") |>
    rename(user_id = `...1`,
           facility_id = `...2`,
           distance = `...3`) |>
    arrange(user_id,
            facility_id) |>
    select(distance) |>
    as.matrix()

test_that("Nearest distances in cpp are same as dplyr method",{
    expect_equal(near_dplyr_test,
                           near_cpp_test)
    })
