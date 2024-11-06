#' (Internal) Summarise maxcovr relocation model with facility and user info
#'
#' `extract_mc_results_relocation` takes a fitted max_coverage object and
#'   returns useful summary information from the model, specifically for the
#'   relocation method.
#'
#' @param x the fitted model from max_coverage_relocation
#'
#' @return a list containing multiple dataframes summarising the model

extract_mc_results_relocation <- function(x){

    if (x$solver == "glpk") {
        solution <- x$solution$solution
    }

    if (x$solver == "lpSolve") {
        solution <- x$solution$solution
    }

    if (x$solver == "gurobi") {
        solution <- x$solution$x
    }

    J <- nrow(x$A)
    I <- ncol(x$A)

    n_existing <- nrow(x$existing_facility)
    n_proposed <- nrow(x$proposed_facility)
    n_facilities <- (nrow(x$existing_facility) + nrow(x$proposed_facility))
    n_existing_removed <- sum(solution[1:n_existing] == 0)

    # how many additional proposed ones were selected?
    n_existing_1 <- n_existing + 1

    # how many facilities were chosen?
    n_proposed_chosen <- sum(solution[n_existing_1:n_facilities])

    # which were moved?
    which_existing_removed <-
        x$existing_facility[which(solution[1:n_existing] == 0), ]

    # create bits to get the right vector size out for the users affected
    n_bit_3 <- n_existing + n_proposed + 1
    n_bit_4 <- length(solution)

    # number of users affected
    n_users_affected <- sum(solution[n_bit_3:n_bit_4])

    # which facilities are to be used
    facility_solution <- solution[1:I]

    facility_id <- readr::parse_number(colnames(x$A))

    # which facilities are selected?
    facility_temp <- tibble::tibble(facility_id = facility_id,
                                    facility_chosen = facility_solution) |>
        dplyr::filter(facility_chosen == 1)

    facility_selected <- dplyr::bind_rows(x$existing_facility,
                                          x$proposed_facility) |>
        dplyr::mutate(facility_id = facility_id) |>
        dplyr::filter(facility_id %in% facility_temp$facility_id) |>
        # facility_id is not needed anymore
        dplyr::select(-facility_id)

    # which users are affected
    user_solution <- solution[c(I + 1):c(I + J)]

    user_temp <- tibble::tibble(user_id = x$user_id,
                                user_chosen = user_solution) |>
        dplyr::filter(user_chosen == 1)

    user_affected <- dplyr::left_join(user_temp,
                                      x$existing_user,
                                      by = "user_id")

    # Return more summaries.

    # NOTE: I really should use `nearest`
    facility_sum_prep <- dplyr::bind_rows(facility_selected,
                                          x$existing_facility) |>
        mc_mat_prep()

    user_sum_prep <- mc_mat_prep(x$existing_user)

    dist_sum_df <- nearest_facility_dist(facility = facility_sum_prep,
                                         user = user_sum_prep) |>
        tibble::as_tibble(.name_repair = "unique_quiet") |>
        dplyr::rename(user_id = `...1`,
                      facility_id = `...2`,
                      distance = `...3`) |>
        dplyr::mutate(is_covered = (distance <= x$distance_cutoff))

    dist_sum_df_nrow <- nrow(dist_sum_df)
    model_coverage <-  dist_sum_df |>
        dplyr::summarise(total_cost = as.numeric(x$cost_total),
                         install_cost = as.numeric(x$cost_install),
                         cost_removal = as.numeric(x$cost_removal),
                         n_proposed_chosen = n_proposed_chosen,
                         n_existing_removed = n_existing_removed,
                         distance_within = as.numeric(x$distance_cutoff),
                         n_cov = sum(is_covered),
                         pct_cov = (sum(is_covered) / dist_sum_df_nrow),
                         n_not_cov =  (sum(is_covered == 0)),
                         pct_not_cov = (sum(is_covered == 0) / dist_sum_df_nrow),
                         dist_avg = mean(distance),
                         dist_sd = stats::sd(distance))

    # add the original coverage
    existing_coverage <- x$existing_facility |>
        nearest(x$existing_user) |>
        dplyr::mutate(is_covered = (distance <= x$distance_cutoff))

    nrow_existing_coverage <- nrow(existing_coverage)

    existing_coverage_summary <- existing_coverage |>
        dplyr::summarise(distance_within = as.numeric(x$distance_cutoff),
                         n_cov = sum(is_covered),
                         pct_cov = (sum(is_covered) / nrow_existing_coverage),
                         n_not_cov =  (sum(is_covered == 0)),
                         pct_not_cov = (sum(is_covered == 0) / nrow_existing_coverage),
                         dist_avg = mean(distance),
                         dist_sd = stats::sd(distance))

    summary_coverage <- dplyr::bind_rows(existing_coverage_summary,
                                         model_coverage)

    # which proposed facilities had a facility installed?
    is_installed_prep <- solution[n_existing_1:n_facilities]

    # update proposed facilities with this info
    x$proposed_facility <- x$proposed_facility |>
        dplyr::mutate(is_installed = is_installed_prep)

    # which existing facilities were relocated?
    is_relocated <- !solution[1:n_existing]

    # update existing facilities with information about relocation
    x$existing_facility <- x$existing_facility |>
        dplyr::mutate(is_relocated = is_relocated)

    # which users were affected?
    is_covered <- solution[n_bit_3:n_bit_4]

    # update users with information about relocation
    x$existing_user <- x$existing_user |>
        dplyr::mutate(is_covered = is_covered)

    res <- tibble::tibble(
        # augmented information about each incoming dataframe
        user = list(x$existing_user),
        existing_facility = list(x$existing_facility),
        proposed_facility = list(x$proposed_facility),

        # basically existing_facility and proposed facility
        facilities_selected = list(facility_selected),

        # simple summary info
        model_coverage = list(model_coverage),
        existing_coverage = list(existing_coverage_summary),
        summary = list(summary_coverage),

        # model_call stuff
        solution_vector = list(solution),
        total_cost = list(x$cost_total),
        distance_cutoff = list(x$distance_cutoff),
        solver_used = list(x$solver),
        model_call = list(x$model_call)

        )
        # The user + facility solution could perhaps be provided in another
        # function to extract the working parts of the optimisation.

        class(res) <- c("maxcovr_relocation",class(res))

        return(res)

}

# In the future, using results extract_mc_result will give the output of
# max_coverage a class and then extract_mc_result will be an S3 method where
# the output in this case of relocation will be something like "mc_relocation"
# as opposed to the other one, which might be "mc_not_relocate", or something.
