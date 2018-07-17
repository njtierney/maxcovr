#' Solve the Maximal Covering Location Problem
#'
#' `max_coverage` solves the binary optimisation problem known as the
#'   "maximal covering location problem" as described by Church
#'   (http://www.geo .ucsb.edu/~forest/G294download/MAX_COVER_RLC_CSR.pdf).
#'   This package was implemented to make it easier to solve this problem in the
#'   context of the research initially presented by Chan et al
#'   (http://circ.ahajournals.org/content/127/17/1801.short) to identify ideal
#'   locations to place AEDs.
#'
#' @param existing_facility data.frame containing the facilities that are
#'   already in existing, with columns names lat, and long.
#' @param proposed_facility data.frame containing the facilities that are
#'   being proposed, with column names lat, and long.
#' @param user data.frame containing the users of the facilities, along with
#'   column names lat, and long.
#' @param distance_cutoff numeric indicating the distance cutoff (in metres)
#'   you are interested in. If a number is less than distance_cutoff, it will be
#'   1, if it is greater than it, it will be 0.
#' @param n_added the maximum number of facilities to add.
#' @param solver character "glpk" (default) or "lpSolve". "gurobi" is currently
#'   in development, see <https://github.com/njtierney/maxcovr/issues/25>
#'
#' @return dataframe of results
#'
#' @examples
#'
#' library(dplyr)
#'
#' # already existing locations
#' york_selected <- york %>% filter(grade == "I")
#'
#' # proposed locations
#' york_unselected <- york %>% filter(grade != "I")
#'
#' mc_result <- max_coverage(existing_facility = york_selected,
#'                           proposed_facility = york_unselected,
#'                           user = york_crime,
#'                           distance_cutoff = 100,
#'                           n_added = 20)
#'
#' mc_result
#'
#' summary(mc_result)
#'
#' # get the facilities chosen
#' mc_result$facility_selected
#'
#' # get the users affected
#' mc_result$user_affected
#'
#' # get the summaries
#' mc_result$summary
#'
#' @export
max_coverage <- function(existing_facility = NULL,
                         proposed_facility,
                         user,
                         distance_cutoff,
                         n_added,
                         solver = "glpk"){

    # turn existing_facility into a matrix suitable for cpp
    existing_facility_cpp <- existing_facility %>%
        dplyr::select(lat, long) %>%
        as.matrix()

    user_cpp <- user %>%
        dplyr::select(lat, long) %>%
        as.matrix()

    dat_nearest_dist <-
        nearest_facility_dist(facility = existing_facility_cpp,
                              user = user_cpp)

    # make nearest dist into dataframe
    # leave only those not covered
    dat_nearest_no_cov <- dat_nearest_dist %>%
        dplyr::as_data_frame() %>%
        dplyr::rename(user_id = V1,
                      facility_id = V2,
                      distance = V3) %>%
        dplyr::filter(distance > distance_cutoff) # 100m is distance_cutoff

    # give user an index
    user <- user %>% dplyr::mutate(user_id = 1:n())

    # create the "not covered" set of data with a join
    user_not_covered <- dat_nearest_no_cov %>%
        dplyr::left_join(user,
                         by = "user_id")

    # Take original user list, the full set of crime, or ohcas, etc
    proposed_facility_cpp <- proposed_facility %>%
        dplyr::select(lat, long) %>%
        as.matrix()

    # update user to be the new users, those who are not covered
    user_cpp <- user_not_covered %>%
        dplyr::select(lat, long) %>%
        as.matrix()

    A <- maxcovr::binary_matrix_cpp(facility = proposed_facility_cpp,
                                    user = user_cpp,
                                    distance_cutoff = distance_cutoff)

    colnames(A) <- 1:nrow(proposed_facility)

    user_id_list <- 1:nrow(user_not_covered)

    Nx <- nrow(A)
    Ny <- ncol(A)
    N <- n_added
    c_vec <- c(rep(0, Ny), rep(1, Nx))
    d_vec <- c(rep(1, Ny), rep(0, Nx))
    Aeq <- d_vec
    beq <- N

    # this is a line to optimise with cpp
    Ain <- cbind(-A, diag(Nx))
    bin <- matrix(rep(0, Nx), ncol = 1)

    # matrix of numeric constraint coefficients,
    # one row per constraint
    # one column per variable
    constraint_matrix <- rbind(Ain, Aeq)
    rhs_matrix <- rbind(bin, beq)

    # this is another line to optimise with c++
    constraint_directions <- c(rep("<=", Nx), "==")

    # capture user input
    model_call <- match.call()

    if (solver == "lpSolve") {

        solution <- lpSolve::lp(
            direction = "max",
            objective.in = c_vec, # objective_in,
            const.mat = constraint_matrix,
            const.dir = constraint_directions,
            const.rhs = rhs_matrix, # constraint_rhs,
            transpose.constraints = TRUE,
            all.bin = TRUE,
            num.bin.solns = 1,
            use.rw = TRUE
            )

        }

    if (solver == "glpk") {

        solution <- Rglpk::Rglpk_solve_LP(
            obj = c_vec,
            mat = constraint_matrix,
            dir = constraint_directions,
            rhs = rhs_matrix,
            bounds = NULL,
            types = "B",
            max = TRUE
        )
    }

    if (solver == "gurobi") {
        if (!requireNamespace("gurobi", quietly = TRUE)) {
            stop(
                "Make sure that you have installed the Gurobi software and accompanying Gurobi R package, more details at https://www.gurobi.com/documentation/7.0/refman/r_api_overview.html"
            )

        }

        constraint_directions_gurobi <- c(rep("<=", Nx), "=")

        model <- list()
        model$A <- constraint_matrix
        model$obj <- c_vec
        model$sense <- constraint_directions_gurobi
        model$rhs <- rhs_matrix
        model$vtype <- "B"
        model$modelsense <- "max"
        solution <- gurobi::gurobi(model)

    }

    x <- list(
        # #add the variables that were used here to get more info
        existing_facility = existing_facility,
        proposed_facility = proposed_facility,
        distance_cutoff = distance_cutoff,
        existing_user = user,
        user_not_covered = user_not_covered,
        n_added = n_added,
        A = A,
        user_id = user_id_list,
        solution = solution,
        model_call = model_call
    )

    model_result <- extract_mc_results(x)

    return(model_result)

    } # end of function
