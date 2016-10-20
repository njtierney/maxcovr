#' max_coverage
#'
#' max_coverage solves the binary optimisation problem known as the "maximal covering location problem" as described by Church (http://www.geog.ucsb.edu/~forest/G294download/MAX_COVER_RLC_CSR.pdf). This package was implemented to make it easier to solve this problem in the context of the research initially presented by Chan et al (http://circ.ahajournals.org/content/127/17/1801.short) to identify ideal locations to place AEDs.
#'
#' @param A is a spread data matrix for all of the distances, it is obtained using facility_user_dist, then facilit_user_indic.
#' @param facility data.frame containing an ohca_id, lat, and long
#' @param user data.frame containing an aed_id, lat, and long
#' @param num_aed is the maximum number of AEDs that can be found
#' @param n_solutions is the number of possible solutions to be returned. Default value is set to 1.
#'
#' @return returns
#' @export
#'
max_coverage <- function(A,
                         facility,
                         user,
                         num_aed,
                         n_solutions = 1){

    # just to make it clear:
    # - facility = aed
    # - user = ohca
    # A <- facility_user_indic(facility = facility,
    #                          user = user,
    #                          dist_indic = dist_indic) # 100m

    # hang on to the list of OHCA ids
    user_id_list <- A[,"user_id"]

    # drop ohca_id
    A <- A[ ,-1]

    # A is a matrix containing 0s and 1s
    # 1 indicates that the OHCA in row I is covered by an AED in location J

J <- nrow(A)
I <- ncol(A)

Nx <- nrow(A)
Ny <- ncol(A)
N <- num_aed

# c <- -[zeros(Ny,1); ones(Nx,1)];
c <- c(rep(0, Ny), rep(1,Nx))

# d <- [ones(1,Ny) zeros(1,Nx)];
d <- c(rep(1, Ny), rep(0,Nx))

# Aeq <- d;
# Aeq <- c # not d
Aeq <- d # not c # this is as of 2016/08/19 - unfortunately Ruth's suggestion
# did not work
# beq <- N;
beq <- N

# Ain <- [-A eye(Nx)];
Ain <- cbind(-A, diag(Nx))

# bin <- zeros(Nx,1);
bin <- matrix(rep(0,Nx),
              ncol = 1)

# matrix of numeric constraint coefficients,
# one row per constraint
# one column per variable
constraint_matrix <- rbind(Ain, Aeq)

rhs_matrix <- rbind(bin, beq)

constraint_directions <- c(rep("<=", Nx), "==")

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
        facility = facility,
        user = user,
        # dist_indic = dist_indic,
        num_aed = num_aed,
        n_solutions = n_solutions,
        A = A,
        user_id = user_id_list,
        lp_solution = lp_solution
    )

model_result <- extract_mc_results(x)

return(model_result)

} # end of function
