library(tibble)

dist_cpp <- spherical_distance_cpp(lat1 = 46.19616,
                                             long1 = 8.731278,
                                             lat2 = 46.16850,
                                             long2 = 9.004392)

dist_maxcovr <- spherical_distance(lat1 = 46.19616,
                                            long1 = 8.731278,
                                            lat2 = 46.16850,
                                            long2 = 9.004392)

test_that("Distances calculated by sperical_distance and spherical_distance_cpp are the same in c++ and R versions",{
    expect_equal(dist_cpp, dist_maxcovr)
})

facility_test_cpp <- as.matrix(tribble(
    ~lat_facility, ~long_facility, ~facility_id, ~key,
    46.19616,      8.731278,           1,     1,
    46.16757,      9.027957,           2,     1
))

user_test_cpp <- as.matrix(tribble(
    ~lat_user, ~long_user, ~key, ~user_id,
    46.16850,  9.004392,     1,       1,
    46.17690,  8.822994,     1,       2,
    46.17690,  8.822994,     1,       3,
    46.17690,  8.822994,     1,       4,
    46.17690,  8.822994,     1,       5,
    46.01372,  8.963890,     1,       6,
    46.15254,  8.773423,     1,       7,
    45.92970,  8.921419,     1,       8,
    45.92970,  8.921419,     1,       9,
    46.00018,  8.946929,     1,      10
))
