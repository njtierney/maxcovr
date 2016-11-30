#' max_coverage
#'
#' max_coverage solves the binary optimisation problem known as the "maximal covering location problem" as described by Church (http://www.geog.ucsb.edu/~forest/G294download/MAX_COVER_RLC_CSR.pdf). This package was implemented to make it easier to solve this problem in the context of the research initially presented by Chan et al (http://circ.ahajournals.org/content/127/17/1801.short) to identify ideal locations to place AEDs.
#'
#' @param existing_facility data.frame containing the facilities that are already in existing, with columns names lat, and long.
#' @param proposed_facility data.frame containing the facilities that are being proposed, with column names lat, and long.
#' @param user data.frame containing the users of the facilities, along with column names lat, and long.
#' @param distance_cutoff numeric indicating the distance cutoff (in metres)
#' you are interested in. If a number is less than distance_cutoff, it will be
#' 1, if it is greater than it, it will be 0.
#' @param n_added the maximum number of facilities to add.
#' @param n_solutions is the number of possible solutions to be returned. Default value is set to 1.
#' @param solver character default is lpSolve, but currently in development is a Gurobi solver
#'
#' @return returns
#'
#' @examples
#'
#' \dontrun{
#' library(maxcovr)
#' library(tidyverse)
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
#'  # get the facilities chosen out
#'  mc_result$facility_selected
#'
#'  # get the users affected out
#'  mc_result$user_affected
#'
#'  # get the summaries out
#'  mc_result$summary
#'
#' }
#'
#'
#' @export
#'
max_coverage <- function(existing_facility = NULL,
                         proposed_facility,
                         user,
                         distance_cutoff,
                         n_added,
                         n_solutions = 1,
                         solver = "lpSolve"){

    # testing...
        # existing_facility = york_selected
        # proposed_facility = york_proposed
        # user = york_crime
        # distance_cutoff = 100
        # n_added = 20
        # n_solutions = 1
    # end testing ....

    # testing for the new use of three dataframes ------------------------------
    # if (is.null(existing_facility)){
    # existing_facility <- york %>% filter(grade == "I")
    #
    # # add an index to the user
    # user <- york_crime %>% mutate(user_id = 1:n())
    #
    # # proposed facility
    # proposed_facility <- york %>% filter(grade != "I")

    # turn existing_facility into a matrix suitable for cpp

    existing_facility_cpp <- existing_facility %>%
        select(lat,long) %>%
        as.matrix()

    user_cpp <- user %>%
        select(lat,long) %>%
        as.matrix()

    dat_nearest_dist <-
        maxcovr::nearest_facility_dist(facility = existing_facility_cpp,
                                       user = user_cpp)

    # make nearest dist into dataframe
    # leave only those not covered
    dat_nearest_no_cov <- dat_nearest_dist %>%
        dplyr::as_data_frame() %>%
        rename(user_id = V1,
               facility_id = V2,
               distance = V3) %>%
        filter(distance > distance_cutoff) # 100m would be distance_cutoff

    # give user an index
    user <- user %>% mutate(user_id = 1:n())

    # join them, to create the "not covered" set of data
    user_not_covered <- dat_nearest_no_cov %>%
        left_join(user,
                  by = "user_id")

    # # this takes the original user list, the full set of crime, or ohcas, etc
    # existing_user <- user
    #
    # # update user to be the new users, those who are not covered
    # user <- user_not_covered

# } # end NULL

    proposed_facility_cpp <- proposed_facility %>%
        select(lat, long) %>%
        as.matrix()

    user_cpp <- user_not_covered %>%
        select(lat, long) %>%
        as.matrix()

    A <- maxcovr::binary_matrix_cpp(facility = proposed_facility_cpp,
                                    user = user_cpp,
                                    distance_cutoff = distance_cutoff)

    facility_names <- sprintf("facility_id_%s",1:nrow(proposed_facility))
    colnames(A) <- facility_names
    # hang on to the list of OHCA ids
    # user_id_list <- A[,"user_id"]
    user_id_list <- 1:nrow(user_not_covered)

    # drop ohca_id
    # A <- A[ ,-1]

    # A is a matrix containing 0s and 1s
    # 1 indicates that the OHCA in row I is covered by an AED in location J

if(solver == "lpSolve"){

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
# for the york data, it takes 0.658 seconds
    lp_solution <- lpSolve::lp(direction = "max",
                           # objective.in = d, # as of 2016/08/19
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
                           num.bin.solns = n_solutions,
                           use.rw = TRUE)

# # there are 148 here
# length(my_soln_2$solution[1:I]) == ncol(A)
#
# # how many are 1?
# sum(my_soln_2$solution[1:I]) == num_aed
#
# # Which positions are 1?
# which(my_soln_2$solution[1:I] == 1)

# note: add a custom class to this object so that I can make sure the next function only accepts it once it has gone through there.

x <- list(
        # #add the variables that were used here to get more info
        existing_facility = existing_facility,
        proposed_facility = proposed_facility,
        distance_cutoff = distance_cutoff,
        existing_user = user,
        user_not_covered = user_not_covered,
        # dist_indic = dist_indic,
        n_added = n_added,
        n_solutions = n_solutions,
        A = A,
        user_id = user_id_list,
        lp_solution = lp_solution
    )

model_result <- maxcovr:::extract_mc_results(x)

return(model_result)

} else if(solver == "gurobi"){

    warning("Make sure that you have installed the Gurobi software and accompanying Gurobi R package")
    # model <- list()
    # model$A          <- matrix(c(1,1,0,0,1,1), nrow=2, byrow=T)
    # model$obj        <- c(1,1,2)
    # model$modelsense <- "max"
    # model$rhs        <- c(1,1)
    # model$sense      <- c('<=', '<=')

    # J <- nrow(A)
    # I <- ncol(A)

    model <- list()

    # set A matrix
    model$A <- A

        Nx <- nrow(A)
        Ny <- ncol(A)
        N <- n_added

        # d <- [ones(1,Ny) zeros(1,Nx)];
        d <- c(rep(1, Ny), rep(0,Nx))
    # c <- -[zeros(Ny,1); ones(Nx,1)];
        # c <- c(rep(0, Ny), rep(1,Nx))
    model$obj <- c(rep(0, Ny), rep(1,Nx))

    model$modelsense <- "max"

        Aeq <- d

        beq <- N

        Ain <- cbind(-A, diag(Nx))

        bin <- matrix(rep(0,Nx), ncol = 1)

    # matrix of numeric constraint coefficients,
    # one row per constraint
    # one column per variable
        # constraint matrix??
    # constraint_matrix <- rbind(Ain, Aeq)
    # rhs_matrix <- rbind(bin, beq)
        model$rhs <- rbind(bin, beq)


    # model$sense      <- c('<=', '<=')
    model$sense <- c(rep("<=", Nx), "==")

    result <- gurobi::gurobi(model)

    return(result)

} # end gurobi

} # end of function
#
# # try out gurobi
# system.time(
# test_cov_lp <- max_coverage(A = a_indic_mat_old,
#                          facility = dat_building,
#                          user = dat_ohca_not_cov,
#                          num_aed = 20)
# )
