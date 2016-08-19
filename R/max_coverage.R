#' max_coverage
#'
#' max_coverage solves the binary optimisation problem known as the "maximum location coverage problem" as described by Church.... This package was implemented to make it easier to solve this problem in the context of the research initially presented by Chan et al to identify ideal locations to place AEDs.
#'
#' @param facility data.frame containing an ohca_id, lat, and long
#' @param user data.frame containing an aed_id, lat, and long
#' @param dist_indic numeric stating the distance in kilometers for facility-user distance
#' @param num_aed is the maximum number of AEDs that can be found
#' @param n_solutions is the number of possible solutions to be returned. Default value is set to 1.
#'
#' @return returns
#' @export
#'
# @examples
max_coverage <- function(facility,
                         user,
                         dist_indic,
                         # A,
                         num_aed,
                         n_solutions = 1){

    A <- facility_user_indic(facility = facility,
                             user = user,
                             dist_indic = dist_indic) # 100m

    # hang on to the list of OHCA ids
    ohca_id_list <- A[,"ohca_id"]

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

# extract results ---------------------------------------------------------
#
# # which AEDs are to be used
# facilities_affected <- lp_solution$solution[1:I]
#
# # which OHCAs
# users_affected <- lp_solution$solution[c(I+1):c(I+J)]
#
# # which facilities are selected?
# facilities_selected <- tibble::tibble(
#     aed_id = readr::parse_number(colnames(A)),
#     aed_chosen = facilities_affected) %>%
#     dplyr::filter(aed_chosen == 1)
#
# users_selected <- tibble::tibble(
#     ohca_id = ohca_id_list,
#     ohca_chosen = users_affected) %>%
#     dplyr::filter(ohca_chosen == 1)
#
# facilities_merge <- facility %>%
#     dplyr::filter(aed_id %in% facilities_selected$aed_id)
# # add this column to help doing a crazy join later
#     # dplyr::mutate(key = 1)
#
# users_merge <- user %>%
#     dplyr::filter(event_id %in% users_selected$ohca_id)
# add this column to help do a crazy join later
    # dplyr::mutate(key = 1)

# facilities_users_merge <- facilities_merge %>%
#     dplyr::left_join(users_merge, by = "key")

# note: add a custom class to this object so that I can make sure the next function only accepts it once it has gone through there.

return(
    list(
        # facilities_affected = facilities_affected,
        # users_affected = users_affected,
        # facilities_merge = facilities_merge,
        # users_merge = users_merge,
        # # facilities_users_merge = facilities_users_merge,
        # #add the variables that were used here to get more info
        facility = facility,
        user = user,
        dist_indic = dist_indic,
        num_aed = num_aed,
        n_solutions = n_solutions,
        A = A,
        ohca_id = ohca_id_list,
        lp_solution = lp_solution
    )
    )

} # end of function
