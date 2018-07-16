#' Maximum Coverage when considering relocation
#'
#' This function adds a relocation step
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
#' @param solver character "glpk" (default) or "lpSolve". "gurobi" is currently
#'   in development, see <https://github.com/njtierney/maxcovr/issues/25>
#' @param cost_install integer the cost of installing a new facility
#' @param cost_removal integer the cost of removing a facility
#' @param cost_total integer the total cost allocated to the project
#' @param return_early logical TRUE if I do not want to run the extraction
#'   process, FALSE if I want to just return the lpsolve model etc.
#'
#' @return dataframe of results
#'
#' @examples
#'
#' \dontrun{
#'
#' library(dplyr)
#' # subset to be the places with towers built on them.
#'
#' york_selected <- york %>% filter(grade == "I")
#'
#' york_unselected <- york %>% filter(grade != "I")
#'
#' # OK, what if I just use some really crazy small data to optimise over.
#'
#' #
#'
#' mc_relocate <-  max_coverage_relocation(existing_facility = york_selected,
#'                                         proposed_facility = york_unselected,
#'                                         user = york_crime,
#'                                         distance_cutoff = 100,
#'                                         cost_install = 5000,
#'                                         cost_removal = 200,
#'                                         cost_total = 600000)
#'
#' mc_relocate
#'
#' summary(mc_relocate)
#'
#' }
#'
#' @export
#'
max_coverage_relocation <- function(existing_facility = NULL,
                                    proposed_facility,
                                    user,
                                    distance_cutoff,
                                    # n_added,
                                    # n_solutions = 1,
                                    cost_install,
                                    cost_removal,
                                    cost_total,
                                    solver = "lpSolve",
                                    return_early = FALSE){

# a little utility function to take the data and get lat/long out and call
# as.matrix on it
# flag - these shouldn't be called lat or long here, the user
# should be able to quote/name the lat/long values
# or there should be a function to find the likely lat/long values
# as in leaflet

    mc_mat_prep <- function(data){
        dplyr::select(data,
                      lat,
                      long) %>%
            as.matrix()
    }

    existing_facility_cpp <-
        binary_matrix_cpp(facility = mc_mat_prep(existing_facility),
                          user = mc_mat_prep(user),
                          distance_cutoff = 100)

    proposed_facility_cpp <-
        binary_matrix_cpp(facility = mc_mat_prep(proposed_facility),
                          user = mc_mat_prep(user),
                          distance_cutoff = 100)

    A <- cbind(
        existing_facility_cpp,
        proposed_facility_cpp
    )

    Nx <- nrow(A)
    Ny <- ncol(A)

    facility_names <- sprintf(
        "facility_id_%s",
        c(1:(nrow(existing_facility) + nrow(proposed_facility)))
        )

    colnames(A) <- facility_names

    user_id_list <- 1:nrow(user)

    # this is N0 is the second model
    N <- nrow(existing_facility)

    c <- c(rep(0, Ny), rep(1,Nx))

    # this is a line to optimise with cpp
    Ain <- cbind(-A, diag(Nx))

    # create the m vector =====================================================
    # this is the vector of costs, which will have the length
    # of the number of rows of y plus the number of x's as 0s

    cost_relocate <- cost_install - cost_removal

    # This is the gain of removing an AED from a location (as opposed to buying)
    # existing facilities will have this cost (ncol(existing_facility_cpp)),
    # proposed facilities don't have this cost (hence, 0s)
    m_under_i <- c(rep(cost_relocate * -1, ncol(existing_facility_cpp)),
                   rep(0,ncol(proposed_facility_cpp)))

    # cost of installing
    # Existing facilities don't have an installation cost (hence, 0s)
    # Proposed facilities have an installation cost, hence cost_install...
    m_over_i <- c(rep(0, ncol(existing_facility_cpp)),
                  rep(cost_install, ncol(proposed_facility_cpp)))

    # trust the algebra
    m_vec <- c(
        m_over_i - m_under_i,
        rep(0, Nx)
        )

    Aeq <- c(rep(1, Ny), rep(0,Nx))

    # matrix of numeric constraint coefficients
    # one row per constraint
    # one column per variable - this is another line to optimise with c++
    constraint_matrix <- rbind(Ain,
                               m_vec,
                               Aeq)

    # constraint_matrix
    bin <- matrix(rep(0,Nx), ncol = 1)

    # trust the algebra
    sum_c_mi <- cost_total - sum(m_under_i)

    beq <- N

    rhs_matrix <- rbind(bin,
                        sum_c_mi,
                        beq)

    # this is another line to optimise with c++
    constraint_directions <- c(rep("<=", Nx),
                               "<=",
                               ">=")

    dat_nearest_dist <- nearest_facility_dist(
        facility = mc_mat_prep(existing_facility),
        user = mc_mat_prep(user)
        )

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

    # join them, to create the "not covered" set of data
    user_not_covered <- dat_nearest_no_cov %>%
        dplyr::left_join(user,
                         by = "user_id")

    # capture the user input for printing
    model_call <- match.call()

    if (solver == "lpSolve") {
        lp_solution <- lpSolve::lp(
            direction = "max",
            objective.in = c,
            const.mat = constraint_matrix,
            const.dir = constraint_directions,
            const.rhs = rhs_matrix,
            transpose.constraints = TRUE,
            all.bin = TRUE,
            num.bin.solns = 1,
            use.rw = TRUE
        )

        x <- list(
            # #add the variables that were used here to get more info
            existing_facility = existing_facility,
            proposed_facility = proposed_facility,
            distance_cutoff = distance_cutoff,
            existing_user = user,
            user_not_covered = user_not_covered,
            n_added = N,
            A = A,
            user_id = user_id_list,
            solution = lp_solution,
            cost_install = cost_install,
            cost_removal = cost_removal,
            cost_total = cost_total,
            model_call = model_call,
            sum_c_mi = sum_c_mi,
            m_vec = m_vec,
            solver = solver
        )

        if (return_early) {
            return(x)
        }

        if (!return_early) {
            model_result <- extract_mc_results_relocation(x)
            return(model_result)
        }

    }

    if (solver == "glpk") {

    glpk_solution <- Rglpk::Rglpk_solve_LP(obj = c,
                                           mat = constraint_matrix,
                                           dir = constraint_directions,
                                           rhs = rhs_matrix,
                                           bounds = NULL,
                                           types = "B",
                                           max = TRUE)

    model_call <- match.call()

    x <- list(
        # #add the variables that were used here to get more info
        existing_facility = existing_facility,
        proposed_facility = proposed_facility,
        distance_cutoff = distance_cutoff,
        existing_user = user,
        user_not_covered = user_not_covered,
        cost_install = cost_install,
        cost_removal = cost_removal,
        cost_total = cost_total,
        model_call = model_call,
        sum_c_mi = sum_c_mi,
        m_vec = m_vec,
        A = A,
        user_id = user_id_list,
        solution = glpk_solution,
        solver = solver
    )

    model_result <- extract_mc_results_relocation(x)

    return(model_result)

    }

    if (solver == "gurobi") {

    if (!requireNamespace("gurobi", quietly = TRUE)) {
        stop("Make sure that you have installed the Gurobi software and accompanying Gurobi R package, more details at https://www.gurobi.com/documentation/7.0/refman/r_api_overview.html")

    }

    model_call <- match.call()

    gurobi_solution <- gurobi::gurobi(
        model = list(
            A = constraint_matrix,
            obj = c,
            sense = constraint_directions,
            rhs = rhs_matrix,
            vtype = "B",
            modelsense = "max"
        )
    )

    x <- list(
        # #add the variables that were used here to get more info
        existing_facility = existing_facility,
        proposed_facility = proposed_facility,
        distance_cutoff = distance_cutoff,
        existing_user = user,
        user_not_covered = user_not_covered,
        cost_install = cost_install,
        cost_removal = cost_removal,
        cost_total = cost_total,
        model_call = model_call,
        sum_c_mi = sum_c_mi,
        m_vec = m_vec,
        A = A,
        user_id = user_id_list,
        solution = gurobi_solution,
        solver = solver
    )

    model_result <- extract_mc_results_relocation(x)

}

} # end function
