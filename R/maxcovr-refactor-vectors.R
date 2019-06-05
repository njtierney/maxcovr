#' (Vectorized) Solve the Maximal Covering Location Problem
#'
#' `max_coverage_vec` does what `max_coverage` does, but it allows for input
#'   of vectors in n_added. This function is currently still under development.
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
#' @param solver character default is lpSolve, but glpk and Gurobi can also be
#'   used.
#'
#' @return dataframe of results
#
# library(dplyr)
# library(maxcovr)
#
# already existing locations
# york_selected <- york %>% filter(grade == "I")
#
# # proposed locations
# york_unselected <- york %>% filter(grade != "I")
#
# existing_facility = york_selected
# proposed_facility = york_unselected
# user = york_crime
# distance_cutoff = 100
# n_added = c(20,40)
# n_solutions = 1

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

# create objective =============================================================
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
user <- user %>% dplyr::mutate(user_id = 1:dplyr::n())

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

# perform extra functions for data munging ===================================
facility_id_list <- 1:nrow(proposed_facility)
colnames(A) <- facility_id_list
user_id_list <- 1:nrow(user_not_covered)

# create constraint matrix ====================================================
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

# create constraint directions ================================================

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

# create right hand side matrix ===============================================

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

# solve the model =============================================================

# at this point, all of the model outputs, independent of being relocation or
# not, are ready for the analysis.

if(solver == "lpSolve"){
    lp_solution <-
        purrr::map(
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

    # solution_result <- vector("list",length(rhs_matrix))
    # this solution_tbl will be different for each solver
    solution_tbl <- tibble::tibble(
        n_added = n_added,
        solution = purrr::map(.x = lp_solution, .f = ~.$solution),
        obj_val = purrr::map_dbl(.x = lp_solution, .f = ~.$objval),
        )
    full_solver <- lp_solution

} else if(solver == "glpk"){

    glpk_solution <- purrr::map(
        .x = rhs_matrix,
        ~Rglpk::Rglpk_solve_LP(obj = c,
                               mat = constraint_matrix,
                               dir = constraint_directions,
                               rhs = .x,
                               bounds = NULL,
                               types = "B",
                               max = TRUE)
    )

    solution_tbl <- tibble::tibble(
        n_added = n_added,
        solution = purrr::map(.x = glpk_solution, .f = ~.$solution),
        obj_val = purrr::map_dbl(.x = lp_solution, .f = ~.$optimum)
    )

    full_solver <- glpk_solution

} else if(solver == "gurobi"){

    if (!requireNamespace("gurobi", quietly = TRUE)) {
      stop("Make sure that you have installed the Gurobi software and
        accompanying Gurobi R package, more details at
        https://www.gurobi.com/documentation/7.0/refman/r_api_overview.html")
    }

    # gurobi is a little precious and doesn't take `==`.
    constraint_directions_gurobi <- c(rep("<=", Nx), "=")

    gurobi_solution <-
        purrr::map(
            .x = rhs_matrix,
            ~gurobi::gurobi(model = list(
                A = constraint_matrix,
                obj = c,
                sense = constraint_directions_gurobi,
                rhs = .x,
                vtypes = "B",
                modelsense = "max"
            ))
            )

    solution_tbl <- tibble::tibble(
        n_added = n_added,
        solution = purrr::map(.x = gurobi_solution, .f = ~.$x),
        obj_val = purrr::map_dbl(.x = gurobi_solution, .f = ~.$objval)
    )
    # ideally I would bind each of the lists to a row for each n_added.
    # but... I can't work out how to do that.
    full_solver <- gurobi_solution

} else{
    message("unknown solver", solver,
            "specified, please enter 'lpSolve', 'glpk', or 'gurobi' ")
}

# general note about solvers,
# I want to split the output

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
    # A = A,
    # instead of A, return the dimesions
    nrow_A = Nx,
    ncol_A = Ny,
    user_id = user_id_list,
    solution_tbl = solution_tbl,
    full_solver = full_solver
    # model_call = model_call
)

# extract the model results ===================================================

# this is a place where I'll need to generalise it into separate function for
# relocation and fixed methods.

# get the dimenions from the matrix
J <- x$nrow_A
I <- x$ncol_A

# which AEDs are to be used
facility_solution <- x$solution_tbl$solution[[1]][1:I]

# which facilities are selected?
facility_temp <- tibble::tibble(
    # get the facility ids
    facility_id = facility_id_list,
    facility_chosen = facility_solution) %>%
    dplyr::filter(facility_chosen == 1)

facility_selected <- x$proposed_facility %>%
    dplyr::mutate(facility_id = facility_id_list) %>%
    dplyr::filter(facility_id %in% facility_temp$facility_id) %>%
    # drop facility_id as it is not needed anymore
    dplyr::select(-facility_id)

# which OHCAs are affected
user_solution <- x$solution_tbl$solution[[1]][c(I+1):c(I+J)]

user_temp <- tibble::tibble(
    user_id = x$user_id,
    user_chosen = user_solution) %>%
    dplyr::filter(user_chosen == 1)

user_affected <- dplyr::left_join(user_temp,
                                  x$user_not_covered,
                                  by = "user_id")

# now to return some more summaries ...

# NOTE: I really should use `nearest`
facility_sum_prep <- dplyr::bind_rows(facility_selected,
                                      x$existing_facility) %>%
    dplyr::select(lat,long) %>%
    as.matrix()

user_sum_prep <- x$existing_user %>%
    dplyr::select(lat,long) %>%
    as.matrix()

dist_sum_df <-
    maxcovr::nearest_facility_dist(facility = facility_sum_prep,
                                   user = user_sum_prep) %>%
    dplyr::as_data_frame() %>%
    dplyr::rename(user_id = V1,
                  facility_id = V2,
                  distance = V3) %>%
    dplyr::mutate(is_covered = (distance <= x$distance_cutoff))

model_coverage <- dist_sum_df %>%
    dplyr::summarise(n_added = as.numeric(x$n_added),
                     distance_within = as.numeric(x$distance_cutoff),
                     n_cov = sum(is_covered),
                     pct_cov = (sum(is_covered) / nrow(.)),
                     n_not_cov =  (sum(is_covered == 0)),
                     pct_not_cov = (sum(is_covered == 0) / nrow(.)),
                     dist_avg = mean(distance),
                     dist_sd = stats::sd(distance))

# add the original coverage
existing_coverage <- x$existing_facility %>%
    nearest(x$existing_user) %>%
    dplyr::mutate(is_covered = (distance <= x$distance_cutoff)) %>%
    dplyr::summarise(n_added = 0,
                     distance_within = as.numeric(x$distance_cutoff),
                     n_cov = sum(is_covered),
                     pct_cov = (sum(is_covered) / nrow(.)),
                     n_not_cov =  (sum(is_covered == 0)),
                     pct_not_cov = (sum(is_covered == 0) / nrow(.)),
                     dist_avg = mean(distance),
                     dist_sd = stats::sd(distance))

# add a summary coverage
summary_coverage = dplyr::bind_rows(existing_coverage,
                                    model_coverage)


res <- tibble::tibble(
    facility_selected = list(facility_selected),
    user_affected = list(user_affected),
    model_coverage = list(model_coverage),
    existing_coverage = list(existing_coverage),
    summary = list(summary_coverage),
    n_added = list(x$n_added),
    distance_cutoff = list(x$distance_cutoff),
    model_call = list(x$model_call)
)

# current thoughts on a new layout:

    # tibble::tibble(
    #     n_added,
    #     facility_selected,
    #     users_affected,
    #     distance_cutoff,
    #     n_cov, # summary?
    #     pct_cov, # summary?
    #     dist_avg, # summary?
    #     dist_sd, # summary?
    #     solver_output,
    #     model_call
    # )

# Where the model will return two rows by default,
# the first row being the existing coverage, and will either contain 0, NULL,
# or the relevant data for the "initial state" so here
# facility_selected = NULL
# ... but then what about the rest of the data?
# I don't want to repeat the existing_summary for each row.
# perhaps it could instead do something else, like return one mega cool tibble
# and then return other information

# not really sure if I need to provide the user + facility solution
# but perhaps I could provide this in another function to extract
# the working parts of the optimisation
# user_solution = user_solution,
# facility_solution = facility_solution,
# facilities_users_merge = facilities_users_merge,
#add the variables that were used here to get more info

# res <- c(class(res),"maxcovr_relocation")
class(res) <- c("maxcovr",class(res))
# class(res) <- c("maxcovr_relocation")

return(res)

}

# return the model ===========================================================

# model_result <- extract_mc_results_2(x)

# return(model_result)


# } # end function

# lpSolve: const.rhs
# glpk: rhs
# gurobi: rhs
