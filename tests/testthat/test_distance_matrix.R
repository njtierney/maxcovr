library(maxcovr)
library(tidyverse)
# my_dist_cpp <- distance_matrix_cpp(facility_test_cpp, user_test_cpp)

context("distance_matrix")

facility_test_cpp <- york %>% select(lat, long) %>% as.matrix()
user_test_cpp <- york_crime %>% select(lat, long) %>% as.matrix()
my_dist_cpp <- distance_matrix_cpp(facility_test_cpp, user_test_cpp)

testthat::test_that("There are no missing values in distance_matrix",{
    testthat::expect_equal(sum(is.na(my_dist_cpp)), 0)
})

# sum(is.na(facility_test_cpp))
# sum(is.na(user_test_cpp))

