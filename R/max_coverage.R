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
# @param n_solutions Number of possible solutions to return. Default is 1.
#' @param solver character default is lpSolve, but glpk and Gurobi can also
#'   be used.
#' @param return_early logical - should I return the object early?
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
#'mc_result <- max_coverage(existing_facility = york_selected,
#'                          proposed_facility = york_unselected,
#'                          user = york_crime,
#'                          distance_cutoff = 100,
#'                          n_added = 20)
#'
#'mc_result
#'
#'summary(mc_result)
#'
#'  # get the facilities chosen
#'  mc_result$facility_selected
#'
#'  # get the users affected
#'  mc_result$user_affected
#'
#'  # get the summaries
#'  mc_result$summary
#'
#'
#' @export
#'
max_coverage <- function(existing_facility = NULL,
                         proposed_facility,
                         user,
                         distance_cutoff,
                         n_added,
                         # n_solutions = 1,
                         solver = "lpSolve",
                         return_early = FALSE){

    # testing...
        # existing_facility = york_selected
        # proposed_facility = york_proposed
        # user = york_crime
        # distance_cutoff = 100
        # n_added = 20
        # n_solutions = 1
    # end testing ....

    # turn existing_facility into a matrix suitable for cpp

    existing_facility_cpp <- existing_facility %>%
        dplyr::select(lat,long) %>%
        as.matrix()

    user_cpp <- user %>%
        dplyr::select(lat,long) %>%
        as.matrix()

    dat_nearest_dist <- nearest_facility_dist(facility = existing_facility_cpp,
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
    # existing_user <- user
    # # update user to be the new users, those who are not covered
    # user <- user_not_covered

    proposed_facility_cpp <- proposed_facility %>%
        dplyr::select(lat, long) %>%
        as.matrix()

    user_cpp <- user_not_covered %>%
        dplyr::select(lat, long) %>%
        as.matrix()

    A <- maxcovr::binary_matrix_cpp(facility = proposed_facility_cpp,
                                    user = user_cpp,
                                    distance_cutoff = distance_cutoff)


    # facility_names <- sprintf("facility_id_%s", 1:nrow(proposed_facility))
    # colnames(A) <- facility_names
    # the facility names aren't used anymore

    colnames(A) <- 1:nrow(proposed_facility)

    # hang on to the list of OHCA ids
    # user_id_list <- A[,"user_id"]

    user_id_list <- 1:nrow(user_not_covered)

    # facility_names <- sprintf("facility_id_%s", 1:nrow(proposed_facility))
    #
    # colnames(A) <- facility_names
    #
    # # hang on to the list of OHCA ids
    # # user_id_list <- A[,"user_id"]
    #
    # user_id_list <- 1:nrow(user_not_covered)

    # drop ohca_id
    # A <- A[ ,-1]

    # A is a matrix containing 0s and 1s
    # 1 indicates that the OHCA in row I is covered by an AED in location J

    # quasi-code for making max_coverage accept n_added as a vector.
    # optim_result_box <- vector("list", length(n_added))
    # n_added <- c(20,40)
    # for(i in n_added){

    # J <- nrow(A)
    # I <- ncol(A)
# profvis::profvis({
    Nx <- nrow(A)
    Ny <- ncol(A)
    N <- n_added
    # N <- n_added[i]

    # c <- -[zeros(Ny,1); ones(Nx,1)];
    c <- c(rep(0, Ny), rep(1,Nx))

    # d <- [ones(1,Ny) zeros(1,Nx)];
    d <- c(rep(1, Ny), rep(0,Nx))

    Aeq <- d
    beq <- N

    # this is a line to optimise with cpp
    Ain <- cbind(-A, diag(Nx))

    bin <- matrix(rep(0,Nx), ncol = 1)

    # matrix of numeric constraint coefficients,
    # one row per constraint
    # one column per variable
    constraint_matrix <- rbind(Ain, Aeq)

    rhs_matrix <- rbind(bin, beq)

    # this is another line to optimise with c++
    constraint_directions <- c(rep("<=", Nx), "==")
# }) # end profvis

    # optim_result_box[[i]] <-

    if(solver == "lpSolve"){
# for the york data, it takes 0.658 seconds
    lp_solution <- lpSolve::lp(direction = "max",
                           objective.in = c,
                           const.mat = constraint_matrix,
                           const.dir = constraint_directions,
                           const.rhs = rhs_matrix,
                           transpose.constraints = TRUE,
                           # int.vec,
                           # presolve = 0,
                           # compute.sens = 0,
                           # binary.vec,
                           # all.int = FALSE,
                           all.bin = TRUE,
                           # scale = 196,
                           # dense.const,
                           num.bin.solns = 1,
                           use.rw = TRUE)


# note: add a custom class to this object so that I can make sure the next function only accepts it once it has gone through there.

    # capture user input
    model_call <- match.call()

    # remove the constraints, as they are too big
    lp_solution[["constraints"]] <- NULL

x <- list(
        # #add the variables that were used here to get more info
        existing_facility = existing_facility,
        proposed_facility = proposed_facility,
        distance_cutoff = distance_cutoff,
        existing_user = user,
        user_not_covered = user_not_covered,
        # dist_indic = dist_indic,
        n_added = n_added,
        # n_solutions = 1,
        A = A,
        user_id = user_id_list,
        lp_solution = lp_solution,
        # remove a constraint!
        model_call = model_call
    )

if(return_early){
    return(x)
} else {

model_result <- extract_mc_results(x)

return(model_result)
}
    } else if(solver == "glpk"){

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
            # dist_indic = dist_indic,
            n_added = n_added,
            # n_solutions = 1,
            A = A,
            user_id = user_id_list,
            glpk_solution = glpk_solution,
            model_call = model_call
        )

        return(x)

    } else if(solver == "gurobi"){

        if (!requireNamespace("gurobi", quietly = TRUE)) {
            stop("Make sure that you have installed the Gurobi software and accompanying Gurobi R package, more details at https://www.gurobi.com/documentation/7.0/refman/r_api_overview.html")

        }

        # gurobi is a little precious and doesn't take `==`.
        constraint_directions_gurobi <- c(rep("<=", Nx), "=")
        model_call <- match.call()
        model <- list()

        model$A <- constraint_matrix
        model$obj <- c
        model$sense <- constraint_directions_gurobi
        model$rhs <- rhs_matrix
        model$vtype <- "B"
        model$modelsense <- "max"

        model

        gurobi_solution <- gurobi::gurobi(model)

        x <- list(
            # #add the variables that were used here to get more info
            existing_facility = existing_facility,
            proposed_facility = proposed_facility,
            distance_cutoff = distance_cutoff,
            existing_user = user,
            user_not_covered = user_not_covered,
            # dist_indic = dist_indic,
            n_added = n_added,
            # n_solutions = 1,
            A = A,
            user_id = user_id_list,
            gurobi_solution = gurobi_solution,
            model_call = model_call
        )

        return(x)

    }

} # end of function
#
# # try out gurobi
# system.time(
# test_cov_lp <- max_coverage(A = a_indic_mat_old,
#                          facility = dat_building,
#                          user = dat_ohca_not_cov,
#                          num_aed = 20)
# )
