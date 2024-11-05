library(dplyr)
library(tibble)
library(tidyr)

set.seed(2019-11-10)
york_selected <- york %>% filter(grade == "I")
york_unselected <- york %>% filter(grade != "I") %>% sample_frac(0.1)

mc_result_glpk <- max_coverage(existing_facility = york_selected,
                               proposed_facility = york_unselected,
                               user = york_crime,
                               distance_cutoff = 100,
                               n_added = 10,
                               solver = "glpk")

test_that("maximum coverage glpk returns the correct names",{
    expect_named(
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

test_that("max_coverage glpk returns has the right class",{
    expect_is(mc_result_glpk, "maxcovr")
    expect_true(is.maxcovr(mc_result_glpk))
    })

test_that("max_coverage glpk returns an integer, not numeric", {
    expect_is(mc_result_glpk$solution[[1]]$solution, "integer")
})


mc_result_lpsolve <- max_coverage(existing_facility = york_selected,
                          proposed_facility = york_unselected,
                          user = york_crime,
                          distance_cutoff = 100,
                          n_added = 10,
                          solver = "lpSolve")

test_that("maximum coverage lpsolve returns the correct names",{
    expect_named(
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

test_that("max_coverage lpsolve returns has the right class",{
    expect_is(mc_result_lpsolve, "maxcovr")
    expect_true(is.maxcovr(mc_result_lpsolve))
    })

test_that("max_coverage lpsolve returns an integer, not numeric", {
    expect_is(mc_result_lpsolve$solution[[1]]$solution, "integer")
})

