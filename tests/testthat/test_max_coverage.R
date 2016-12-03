library(maxcovr)
library(dplyr)
library(tibble)
library(tidyr)

context("max_coverage works")

york_selected <- york %>% dplyr::filter(grade == "I")
york_unselected <- york %>% dplyr::filter(grade != "I")

mc_result <- max_coverage(existing_facility = york_selected,
                          proposed_facility = york_unselected,
                          user = york_crime,
                          distance_cutoff = 100,
                          n_added = 20)

# lapply(mc_result, class)

testthat::test_that("maximum coverage returns the correct names",{
    testthat::expect_named(mc_result, c("facility_selected",
                                        "user_affected",
                                        "model_coverage",
                                        "existing_coverage",
                                        "summary",
                                        "n_added",
                                        "distance_cutoff"))

})

