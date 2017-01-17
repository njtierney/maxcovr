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
                                        "distance_cutoff",
                                        "model_call"))

})
testthat::test_that("max_coverage returns has the right class",{
    testthat::expect_is(mc_result, "maxcovr")
    testthat::expect_true(is.maxcovr(mc_result))
})


mc_relocate <-  max_coverage_relocation(existing_facility = york_selected,
                                        proposed_facility = york_unselected,
                                        user = york_crime,
                                        distance_cutoff = 100,
                                        cost_install = 5000,
                                        cost_relocate = 200,
                                        cost_total = 600000)


testthat::test_that("max_coverage_relocation returns the correct names",{
    testthat::expect_named(mc_relocate, c("facility_selected",
                                        "user_affected",
                                        "which_existing_removed",
                                        "model_coverage",
                                        "existing_coverage",
                                        "summary",
                                        "total_cost",
                                        "distance_cutoff",
                                        "model_call"))

})
testthat::test_that("maximum_coverage_relocation has the right class",{
    testthat::expect_is(mc_relocate, "maxcovr_relocation")
    testthat::expect_true(is.maxcovr_relocation(mc_relocate))
})
