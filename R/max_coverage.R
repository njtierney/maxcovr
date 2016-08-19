#' max_coverage
#'
#' max_coverage solves the binary optimisation problem known as the "maximum location coverage problem" as described by Church.... This package was implemented to make it easier to solve this problem in the context of the research initially presented by Chan et al to identify ideal locations to place AEDs.
#'
#' @param A is a matrix containing 0s and 1s, where 1 indicates that the OHCA in row I is covered by an AED in location J
#' @param num_aed is the maximum number of AEDs that can be found
#' @param n_solutions is the number of possible solutions to be returned. Default value is set to 1.
#'
#' @return returns
#' @export
#'
# @examples
max_coverage <- function(A,
                         num_aed,
                         n_solutions = 1){

J <- nrow(A)
I <- ncol(A)

Nx <- J
Ny <- I
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
                           num.bin.solns = 1,
                           use.rw = TRUE)

# which sites (OHCA incidents) are now covered
# n_incidents_2 <- mc_result$solution[1:ncol(a_mat_200m)]
#
# # the sites/locations (OHCA incidents) that are now covered
# lb_solution$solution[1:ncol(A)]

return(lp_solution)

} # end of function
