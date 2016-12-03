library(tibble)
library(maxcovr)
context("spherical_distance")


dist_cpp <- maxcovr:::spherical_distance_cpp(lat1 = 46.19616,
                                             long1 = 8.731278,
                                             lat2 = 46.16850,
                                             long2 = 9.004392)

dist_maxcovr <- maxcovr::spherical_distance(lat1 = 46.19616,
                                            long1 = 8.731278,
                                            lat2 = 46.16850,
                                            long2 = 9.004392)

test_that("Distances calculated by sperical_distance and spherical_distance_cpp are the same in c++ and R versions",{
    testthat::expect_equal(dist_cpp, dist_maxcovr)
})


facility_test_cpp <- as.matrix(tibble::tribble(
    ~lat_facility, ~long_facility, ~facility_id, ~key,
    46.19616,      8.731278,           1,     1,
    46.16757,      9.027957,           2,     1
))

user_test_cpp <- as.matrix(tibble::tribble(
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


# maxcovr:::spherical_distance_cpp(lat1 = 46.19616, long1 = 9.027957,
#                                  lat2 = 46.00018, long2 = 8.946929)
#
# maxcovr::spherical_distance(lat1 = 46.19616, long1 = 9.027957,
#                             lat2 = 46.00018, long2 = 8.946929)
#
#
# my_dist <- distance_matrix_cpp(facility_test_cpp, user_test_cpp)
#
# my_dist
#
#
# facility_test_maxcovr <- dplyr::as_data_frame(facility_test_cpp)
# facility_test_maxcovr <- dplyr::rename(facility_test_maxcovr,
#                                        lat = lat_facility,
#                                        long = long_facility)
#
# user_test_maxcovr <- dplyr::as_data_frame(user_test_cpp)
# user_test_maxcovr <- dplyr::rename(user_test_maxcovr,
#                                    lat = lat_user,
#                                    long = long_user)
#
# my_dist_regular <- maxcovr::facility_user_dist(facility_test_maxcovr,
#                                                user_test_maxcovr,
#                                                nearest = "both")
# library(tibble)
# options(tibble.print_max = 20, tibble.print_min = 10)
# as_tibble(dplyr::select(my_dist_regular, distance))
#
# microbenchmark::microbenchmark(
#     dist_cpp <- spherical_distance_cpp(lat1 = 46.19616,
#                                        long1 = 8.731278,
#                                        lat2 = 46.16850,
#                                        long2 = 9.004392),
#
#     dist_maxcovr <- maxcovr::spherical_distance(lat1 = 46.19616,
#                                                 long1 = 8.731278,
#                                                 lat2 = 46.16850,
#                                                 long2 = 9.004392),
#     unit = "eps"
#
# )
# # indicator_matrix_cpp(my_new_dist, indic_dist = 100)
#
