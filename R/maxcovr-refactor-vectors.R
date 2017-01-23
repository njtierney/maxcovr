#' (Vectorized) Solve the Maximal Covering Location Problem
#'
#' \code{max_coverage_vec} does what \code{max_coverage} does, but it allows for input of vectors in n_added. This function is currently still under development, as I still need to work out how to understand it.
#'
#' @param existing_facility data.frame containing the facilities that are already in existing, with columns names lat, and long.
#' @param proposed_facility data.frame containing the facilities that are being proposed, with column names lat, and long.
#' @param user data.frame containing the users of the facilities, along with column names lat, and long.
#' @param distance_cutoff numeric indicating the distance cutoff (in metres)
#' you are interested in. If a number is less than distance_cutoff, it will be
#' 1, if it is greater than it, it will be 0.
#' @param n_added the maximum number of facilities to add.
# @param n_solutions Number of possible solutions to return. Default is 1.
#' @param solver character default is lpSolve, but glpk and Gurobi can also be used.
#'
#' @return dataframe of results
#
library(dplyr)
library(maxcovr)
#
# already existing locations
york_selected <- york %>% filter(grade == "I")

# proposed locations
york_unselected <- york %>% filter(grade != "I")

existing_facility = york_selected
proposed_facility = york_unselected
user = york_crime
distance_cutoff = 100
n_added = c(20,40)
n_solutions = 1

#' @export
#'
max_coverage_vec <- function(existing_facility = NULL,
                         proposed_facility,
                         user,
                         distance_cutoff,
                         n_added,
                         # n_solutions = 1,
                         solver = "lpSolve"){

# include some sort of description here about the format we are using
# for the code, and solver, and how we will change it to more sensible names like:

# initially, the model was solved using the syntax:
    # minimize f^t_x subject to:
    # x are integers
    # A . x <= b
    # Aeq . x <= beq
    # lb <= x <= ub

# The arguments have been changed to:
    # objective
    # constraint_matrix
    # constraint_dir
    # rhs

# create objective -------------------------------------------------------------
    # this should be made into a function that would perform different
    # functions based on whether relocation == TRUE

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

# join them, to create the "not covered" set of data
user_not_covered <- dat_nearest_no_cov %>%
    dplyr::left_join(user,
                     by = "user_id")

proposed_facility_cpp <- proposed_facility %>%
    dplyr::select(lat, long) %>%
    as.matrix()

user_cpp <- user_not_covered %>%
    dplyr::select(lat, long) %>%
    as.matrix()

A <- maxcovr::binary_matrix_cpp(facility = proposed_facility_cpp,
                                user = user_cpp,
                                distance_cutoff = distance_cutoff)

Nx <- nrow(A)
Ny <- ncol(A)

# this is the objective
c <- c(rep(0, Ny), rep(1,Nx))

# lpSolve: objective.in # lpSolve
# glpk: obj # glpk
# gurobi: obj # gurobi

# output from this function could be:
    # A
    # Nx
    # Ny
    # c (objective_in)

# perform extra functions for data munging -------------------------------------
facility_id_list <- 1:nrow(proposed_facility)
colnames(A) <- facility_id_list
user_id_list <- 1:nrow(user_not_covered)

# create constraint matrix -----------------------------------------------------
    # this should be made into a function that would perform different
    # functions based on whether relocation == TRUE
    # create_constraint_matrix(..., relocation){

# lpSolve: const.mat
# glpk: mat
# gurobi: A

constraint_matrix <- rbind(
    # Ain <- cbind(-A, diag(Nx))
    cbind(-A, diag(Nx)),
    # d <- c(rep(1, Ny), rep(0,Nx))
    # Aeq <- d
    c(rep(1, Ny), rep(0,Nx))
    )

# create constraint directions -------------------------------------------------

# lpSolve: const.dir
# glpk: dir
# gurobi: sense
# solver = "lpSolve"



if(solver == "gurobi"){

    # gurobi can't handle the ==.
    constraint_directions <- c(rep("<=", Nx), "=")

    # else if it is not gurobi
} else{

constraint_directions <- c(rep("<=", Nx), "==")

} # end else

# create right hand side matrix ------------------------------------------------

# bin <- matrix(rep(0,Nx), ncol = 1)
# rhs_matrix <- rbind(bin, beq)

# create a list of rhs matrices
rhs_matrix <- purrr::map(
    .x = n_added,
    ~rbind(
        # bin <- matrix(rep(0,Nx), ncol = 1)
        matrix(rep(0,Nx), ncol = 1),
        # beq <- N
        n_added = .x
    )
)

if(solver == "lpSolve"){
    lp_solution <-
        purrr::map_df(
            .x = rhs_matrix,
            ~lpSolve::lp(direction = "max",
                    objective.in = c,
                    const.mat = constraint_matrix,
                    const.dir = constraint_directions,
                    const.rhs = .x,
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
        )

    solution_result <- list(
        solution = lp_solution$solution,
        obj_val = lp_solution$objval
    )





} else if(solver == "glpk"){



} else if(solver == "gurobi"){


} else{
    message("unknown solver", solver,
            "specified, please enter 'lpSolve', 'glpk', or 'gurobi' ")
}

# capture user input
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
    lp_solution = lp_solution,
    model_call = model_call
)

model_result <- extract_mc_results_2(x)

return(model_result)


} # end function

    # lpSolve: const.rhs
# glpk: rhs
# gurobi: rhs
