library(maxcovr)
library(dplyr)
library(tibble)
library(tidyr)

context("max_coverage")

york_selected <- york %>% dplyr::filter(grade == "I")
york_unselected <- york %>% dplyr::filter(grade != "I")

mc_result <- max_coverage(existing_facility = york_selected,
                          proposed_facility = york_unselected,
                          user = york_crime,
                          distance_cutoff = 100,
                          n_added = 20)

# lapply(mc_result, class)


testthat::test_that("maximum coverage returns the correct names",{
    testthat::expect_named(
        object = mc_result,
        expected = c(
        "n_added",
        "distance_cutoff",
        "user_affected",
        "augmented_users",
        "facility_selected",
        "model_coverage",
        "existing_coverage",
        "summary",
        "model_call",
        "solution"
        )
        )
    })

testthat::test_that("max_coverage returns has the right class",{
    testthat::expect_is(mc_result, "maxcovr")
    testthat::expect_true(is.maxcovr(mc_result))
    })
