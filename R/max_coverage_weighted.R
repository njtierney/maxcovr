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
#'   column names lat, long, and weight. Weight is the demand weight for each user.
#' @param distance_cutoff numeric indicating the distance cutoff (in metres)
#'   you are interested in. If a number is less than distance_cutoff, it will be
#'   1, if it is greater than it, it will be 0.
#' @param n_added the maximum number of facilities to add.
#' @param d_existing_user Optional distance matrix between existing facilities
#' and users. Default distances are direct (geospherical ellipsoidal) distances;
#' this allows alternative measures such as street-network distances to be
#' submitted (see Examples).
#' @param d_proposed_user Option distance matrix between proposed facilities and
#' users (see Examples).
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
#' # Example of street-network distance calculations
#' \dontrun{
#' library(dodgr)
#' net <- dodgr_streetnet_sf ("york england") %>%
#'     weight_streetnet (wt_profile = "foot")
#'
#' from <- match_points_to_graph (v, york_selected [, c ("long", "lat")])
#' to <- match_points_to_graph (v, york_crime [, c ("long", "lat")])
#' d_existing_user <- dodgr_dists (net, from = from, to = to)
#'
#' from <- match_points_to_graph (v, york_unselected [, c ("long", "lat")])
#' d_proposed_user <- dodgr_dists (net, from = from, to = to)
#'
#' mc_result <- max_coverage(existing_facility = york_selected,
#'                           proposed_facility = york_unselected,
#'                           user = york_crime,
#'                           distance_cutoff = 100,
#'                           n_added = 20,
#'                           d_existing_user = d_existing_user,
#'                           d_proposed_user = d_proposed_user)
#'
#' }
#' @export
max_coverage_weighted <- function(existing_facility,
                         proposed_facility,
                         user,
                         distance_cutoff,
                         n_added,
                         d_existing_user = NULL,
                         d_proposed_user = NULL,
                         solver = "glpk"){

    # give user an ID
    user <- tibble::rowid_to_column(user, var = "user_id")

    # if existing_facility is null, use all users for the next step
    if(is.null(existing_facility)){
      user_not_covered <- user
    }else{
      user_not_covered <- find_users_not_covered(existing_facility,
                                                 user,
                                                 distance_cutoff,
                                                 d_existing_user = d_existing_user)
    }
    
    A <- binary_distance_matrix(facility = proposed_facility,
                                user = user_not_covered,
                                distance_cutoff = distance_cutoff,
                                d_proposed_user = d_proposed_user)

    colnames(A) <- 1:nrow(proposed_facility)
    user_id_list <- 1:nrow(user_not_covered)
    # number of user_not_covered
    Nx <- nrow(A)
    # number of proposed facilities
    Ny <- ncol(A)
    # c_vec represents the user weight. All 1 means all users have the same weight.
    # c_vec <- c(rep(0, Ny), rep(1, Nx))
    c_vec <- c(rep(0, Ny), user_not_covered$weight)
    # d_vec represents the constraint of number of facilities to add
    d_vec <- c(rep(1, Ny), rep(0, Nx))

    # this is a line to optimise with cpp
    Ain <- cbind(-A, diag(Nx))
    bin <- matrix(rep(0, Nx), ncol = 1)

    # matrix of constraint coefs, one row per constraint, one col per variable
    constraint_matrix <- rbind(Ain, d_vec)
    rhs_matrix <- rbind(bin, n_added)

    # Another line to optimise with c++
    constraint_directions <- c(rep("<=", Nx), "==")

    # Solve the problem --------------------------------------------------------

    if (solver == "lpSolve") {
        solution <- lpSolve::lp(direction = "max",
                                objective.in = c_vec, # objective_in,
                                const.mat = constraint_matrix,
                                const.dir = constraint_directions,
                                const.rhs = rhs_matrix, # constraint_rhs,
                                transpose.constraints = TRUE,
                                all.bin = TRUE,
                                num.bin.solns = 1,
                                use.rw = TRUE)

        # coerce to integer to save space
        solution$solution <- as.integer(solution$solution)
        }

    if (solver == "glpk") {
        solution <- Rglpk::Rglpk_solve_LP(obj = c_vec,
                                          mat = constraint_matrix,
                                          dir = constraint_directions,
                                          rhs = rhs_matrix,
                                          bounds = NULL,
                                          types = "B",
                                          max = TRUE)

        # coerce to integer to save space
        solution$solution <- as.integer(solution$solution)
    }

    if (solver == "gurobi") {
        if (!requireNamespace("gurobi", quietly = TRUE)) {
            stop("You must have installed the Gurobi software and accompanying
                 Gurobi R package. For more details, see
                 https://www.gurobi.com/documentation/7.0/refman/r_api_overview.html")
        }

        model <- list()
        model$A <- constraint_matrix
        model$obj <- c_vec
        model$sense <- c(rep("<=", Nx), "=") # constraint directions
        model$rhs <- rhs_matrix
        model$vtype <- "B"
        model$modelsense <- "max"
        solution <- gurobi::gurobi(model)

    }

    # Extract the results ------------------------------------------------------

    # capture user input
    model_call <- match.call()

    x <- list(existing_facility = existing_facility,
              proposed_facility = proposed_facility,
              distance_cutoff = distance_cutoff,
              existing_user = user,
              user_not_covered = user_not_covered,
              n_added = n_added,
              A = A,
              user_id = user_id_list,
              solution = solution,
              model_call = model_call)

    model_result <- extract_mc_results(x)

    return(model_result)

    } # end of function


