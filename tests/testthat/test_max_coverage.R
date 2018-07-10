context("max_coverage")

library(maxcovr)
library(dplyr)
library(tibble)
library(tidyr)


york_selected <- york %>% dplyr::filter(grade == "I")
york_unselected <- york %>% dplyr::filter(grade != "I")

mc_result_glpk <- max_coverage(existing_facility = york_selected,
                          proposed_facility = york_unselected,
                          user = york_crime,
                          distance_cutoff = 100,
                          n_added = 20,
                          solver = "glpk")

# lapply(mc_result, class)


testthat::test_that("maximum coverage glpk returns the correct names",{
    testthat::expect_named(
        object = mc_result_glpk,
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

testthat::test_that("max_coverage glpk returns has the right class",{
    testthat::expect_is(mc_result_glpk, "maxcovr")
    testthat::expect_true(is.maxcovr(mc_result_glpk))
    })

mc_result_lpsolve <- max_coverage(existing_facility = york_selected,
                          proposed_facility = york_unselected,
                          user = york_crime,
                          distance_cutoff = 100,
                          n_added = 20,
                          solver = "lpSolve")

# lapply(mc_result, class)


testthat::test_that("maximum coverage lpsolve returns the correct names",{
    testthat::expect_named(
        object = mc_result_lpsolve,
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

testthat::test_that("max_coverage lpsolve returns has the right class",{
    testthat::expect_is(mc_result_lpsolve, "maxcovr")
    testthat::expect_true(is.maxcovr(mc_result_lpsolve))
    })
