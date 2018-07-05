context("max_coverage_relocation")

library(maxcovr)
library(dplyr)
library(tibble)
library(tidyr)

york_selected <- york %>% dplyr::filter(grade == "I")
york_unselected <- york %>% dplyr::filter(grade != "I")


mc_relocate <-  max_coverage_relocation(existing_facility = york_selected,
                                        proposed_facility = york_unselected,
                                        user = york_crime,
                                        distance_cutoff = 100,
                                        cost_install = 500,
                                        cost_removal = 100,
                                        cost_total = 1000)

# mc_relocate

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
