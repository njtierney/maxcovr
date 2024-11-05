library(dplyr)
library(tibble)
library(tidyr)

set.seed(2019-11-10)
york_selected <- york %>% filter(grade == "I")
york_unselected <- york %>% filter(grade != "I") %>% sample_frac(0.1)

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

test_that("max_coverage_relocation with glpk returns correct names",{
    expect_named(mc_relocate_glpk, mc_table_names)

})

test_that("max_coverage_relocation with lpSolve returns correct names",{
    expect_named(mc_relocate_lpsolve, mc_table_names)

})

test_that("maximum_coverage_relocation with glpk has the right class",{
    expect_is(mc_relocate_glpk, "maxcovr_relocation")
    expect_true(is.maxcovr_relocation(mc_relocate_glpk))
})

test_that("maximum_coverage_relocation with lpSolve has the right class",{
    expect_is(mc_relocate_lpsolve, "maxcovr_relocation")
    expect_true(is.maxcovr_relocation(mc_relocate_lpsolve))
})
