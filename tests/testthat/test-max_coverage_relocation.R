context("max_coverage_relocation")

library(maxcovr)
library(dplyr)
library(tibble)
library(tidyr)

york_selected <- york %>% dplyr::filter(grade == "I")
york_unselected <- york %>% dplyr::filter(grade != "I")

mc_relocate_glpk <-
    max_coverage_relocation(existing_facility = york_selected,
                            proposed_facility = york_unselected,
                            user = york_crime,
                            distance_cutoff = 100,
                            cost_install = 500,
                            cost_removal = 100,
                            cost_total = 1000,
                            solver = "glpk")

mc_relocate_lpsolve <-
    max_coverage_relocation(existing_facility = york_selected,
                            proposed_facility = york_unselected,
                            user = york_crime,
                            distance_cutoff = 100,
                            cost_install = 500,
                            cost_removal = 100,
                            cost_total = 1000,
                            solver = "lpSolve")

mc_table_names <- c("user",
                    "existing_facility",
                    "proposed_facility",
                    "facilities_selected",
                    "model_coverage",
                    "existing_coverage",
                    "summary",
                    "solution_vector",
                    "total_cost",
                    "distance_cutoff",
                    "solver_used",
                    "model_call")

testthat::test_that("max_coverage_relocation with glpk returns correct names",{
    testthat::expect_named(mc_relocate_glpk, mc_table_names)

})

testthat::test_that("max_coverage_relocation with lpSolve returns correct names",{
    testthat::expect_named(mc_relocate_lpsolve, mc_table_names)

})

testthat::test_that("maximum_coverage_relocation with glpk has the right class",{
    testthat::expect_is(mc_relocate_glpk, "maxcovr_relocation")
    testthat::expect_true(is.maxcovr_relocation(mc_relocate_glpk))
})

testthat::test_that("maximum_coverage_relocation with lpSolve has the right class",{
    testthat::expect_is(mc_relocate_lpsolve, "maxcovr_relocation")
    testthat::expect_true(is.maxcovr_relocation(mc_relocate_lpsolve))
})
