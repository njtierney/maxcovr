#' Maximum Coverage when considering relocation
#'
#' This function adds a relocation step
#'
#' @param existing_facility data.frame containing the facilities that are already in existing, with columns names lat, and long.
#' @param proposed_facility data.frame containing the facilities that are being proposed, with column names lat, and long.
#' @param user data.frame containing the users of the facilities, along with column names lat, and long.
#' @param distance_cutoff numeric indicating the distance cutoff (in metres)
#' you are interested in. If a number is less than distance_cutoff, it will be
#' 1, if it is greater than it, it will be 0.
#  @param n_added the maximum number of facilities to add.
# @param n_solutions Number of possible solutions to return. Default is 1.
#' @param solver character default is lpSolve, but currently in development is a Gurobi solver, see issue #25 : \url{https://github.com/njtierney/maxcovr/issues/25}
#' @param cost_install integer the cost of installing a new facility
#' @param cost_removal integer the cost of removing a facility
#' @param cost_total integer the total cost allocated to the project
#'
#' @return dataframe of results
#'
#' @export
#'
#' @examples
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
#'                                         cost_relocate = 200,
#'                                         cost_total = 600000)
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
                                    solver = "lpSolve"){

# the A matrix that I feed here will be the combination of the
# existing AED locations and the potential AED locations.

    # using the AED data
#
#     library(tidyverse)
#     library(feather)
#     library(maxcovr)
#
#     # wd <- rprojroot::find_rstudio_root_file()
#
        #     dat_aed <- read_feather("/Users/tierneyn/Google Drive/ALL THE THINGS/PhD/code/R/smaed/analysis/outputs/01_tidy_output/dat_aed.feather")
#
        #     dat_building <- read_feather("/Users/tierneyn/Google Drive/ALL THE THINGS/PhD/code/R/smaed/analysis/outputs/01_tidy_output/dat_building.feather")
#
        #     dat_ohca <- read_feather("/Users/tierneyn/Google Drive/ALL THE THINGS/PhD/code/R/smaed/analysis/outputs/01_tidy_output/dat_ohca.feather")


        # mc_model_relocate <- max_coverage_relocation(existing_facility = dat_aed,
        # existing_facility = dat_aed
        # proposed_facility = dat_building
        # user = dat_ohca
        # distance_cutoff = 100
        # cost_install = 2500
        # cost_relocate = 700
        # cost_total = 50000

    # mc_model_aed_20$summary



    ## # library(dplyr)
    ## # # subset to be the places with towers built on them.
    ## #     york_selected <- york %>% filter(grade == "I")
    ## #     york_unselected <- york %>% filter(grade != "I")
    ## #     # OK, what if I just use some really crazy small data to optimise over.
    ## #     #
    ## #     existing_facility = york_selected
    ## # proposed_facility = york_unselected
    ## # user = york_crime
    ## # distance_cutoff = 100
    ## # cost_install = 5000
    ## # cost_relocate = 200
    ## # cost_total = 600000

# test data set using fake data ....
    #     library(dplyr)
    #
    #     existing_facility =  matrix(data = c(0, 0,
    #                                          0, 1,
    #                                          0, 0,
    #                                          1, 0,
    #                                          1, 0,
    #                                          0, 1),
    #                                 nrow = 6,
    #                                 ncol = 2,
    #                                 byrow = TRUE)
    #
    #     proposed_facility = matrix(data = c(1, 0, 0, 0, 0,
    #                                         0, 1, 1, 0, 0,
    #                                         0, 0, 0, 1, 1,
    #                                         1, 1, 0, 0, 0,
    #                                         0, 1, 0, 0, 0,
    #                                         0, 0, 1, 1, 0),
    #                                nrow = 6,
    #                                ncol = 5,
    #                                byrow = TRUE)
    #     # user = york_crime
    #     distance_cutoff = 100
    #     n_added = nrow(existing_facility) # should be #AEDs, # existing facilities
    #     n_solutions = 1
    #     cost_install = 5000
    #     cost_relocate = 200
    #     cost_total = 10^6 # some super large cost
    #
    # A <- cbind(existing_facility, proposed_facility)
    #
    # A

# end testing with fake data....

#

    # a little utility function to take the data and then get the lat/long
    # out and call as.matrix on it

    mc_mat_prep <- function(data){
        dplyr::select(data,lat,long) %>%
            as.matrix()
    }

    existing_facility_cpp <-
        binary_matrix_cpp(facility = mc_mat_prep(existing_facility),
                          user = mc_mat_prep(york_crime),
                          distance_cutoff = 100)

    proposed_facility_cpp <-
        binary_matrix_cpp(facility = mc_mat_prep(proposed_facility),
                          user = mc_mat_prep(york_crime),
                          distance_cutoff = 100)

    A <- cbind(
        existing_facility_cpp,
        proposed_facility_cpp
    )


Nx <- nrow(A)

# Nx

Ny <- ncol(A)

facility_names <- sprintf("facility_id_%s",
                          c(1:(nrow(existing_facility) + nrow(proposed_facility))))

colnames(A) <- facility_names

# hang on to the list of OHCA ids
# user_id_list <- A[,"user_id"]

user_id_list <- 1:nrow(user)

# Ny
# n_added <-

# this is N0 is the second model
N <- nrow(existing_facility)

# N

# N <- n_added[i]

c <- c(rep(0, Ny), rep(1,Nx))

# c

# this is a line to optimise with cpp
Ain <- cbind(-A, diag(Nx))

# Ain



# ------------------------


# create the m vector ----------------------------------------------------------

# this is the vector of costs, which will have the length
# of the number of rows of y plus the number of x's as 0s

# if using the testing data

# these are for the testing data
# m_vec <- c(
#     rep(cost_relocate*-1, ncol(existing_facility)),
#     rep(cost_install, ncol(proposed_facility)),
#     rep(0, Nx)
# )


cost_relocate <- cost_install - cost_removal

# so this is the gain of removing an AED from a location (as opposed to buying)
# the existing facilities will have this cost (ncol(existing_facility_cpp)),
# and then the proposed facilities don't have this cost (hence, 0s)
m_under_i <- c(rep(cost_relocate * -1, ncol(existing_facility_cpp)),
               rep(0,ncol(proposed_facility_cpp)))


# cost of installing
# The existing facilities don't have an installation cost (hence, 0s)
# the proposed facilities have an installation cost, hence cost_install...
m_over_i <- c(rep(0, ncol(existing_facility_cpp)),
              rep(cost_install, ncol(proposed_facility_cpp)))

# m_over_i <- rep(cost_install, ncol(proposed_facility_cpp))

# m_vec <- c(
#     # these two are for the real data
#     m_under_i,
#     m_over_i,
#     rep(0, Nx)
# )

# trust in the algebra.
m_vec <- c(
    m_over_i - m_under_i,
    rep(0, Nx)
)

# m_vec

# just gets assigned to Aeq
# d <- c(rep(1, Ny), rep(0,Nx))
# d

Aeq <- c(rep(1, Ny), rep(0,Nx))

# Aeq

# matrix of numeric constraint coefficients,
# one row per constraint
# one column per variable
constraint_matrix <- rbind(Ain,
                           m_vec,
                           Aeq)

# constraint_matrix

bin <- matrix(rep(0,Nx), ncol = 1)

# bin



# this is sum_{i = 1}^I
# sum_c_mi <- cost_total - sum(abs(m_vec[m_vec<0]))

# 2017/01/18 - remove the absolute, to corretly penalise reinstallation
# sum_c_mi <- cost_total - sum(m_vec[m_vec<0])

# trust the algebra.
sum_c_mi <- cost_total - sum(m_under_i)

# sum_c_mi

beq <- N

# beq

rhs_matrix <- rbind(bin,
                    sum_c_mi,
                    beq)

# rhs_matrix

# this is another line to optimise with c++
constraint_directions <- c(rep("<=", Nx),
                           "<=",
                           ">=")

# tail(constraint_directions)
# }) # end profvis

# constraint_directions

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
                           num.bin.solns = 1,
                           use.rw = TRUE)

# determing the users not covered

dat_nearest_dist <- nearest_facility_dist(facility = mc_mat_prep(existing_facility),
                                          user = mc_mat_prep(user))

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

# / end determining users not covered

# capture the user input for printing
model_call <- match.call()


# model_call <- list(existing_facility = paste(deparse(existing_facility)),
#                    proposed_facility = quote(proposed_facility),
#                    user = quote(user),
#                    distance_cutoff = quote(distance_cutoff),
#                    n_added = quote(n_added),
#                    n_solutions = quote(n_solutions),
#                    cost_install = quote(cost_install), # = NULL?
#                    cost_relocate = quote(cost_relocate), # = NULL?
#                    cost_total = quote(cost_total), # = NULL?
#                    solver = quote(solver))

x <- list(
    # #add the variables that were used here to get more info
    existing_facility = existing_facility,
    proposed_facility = proposed_facility,
    distance_cutoff = distance_cutoff,
    existing_user = user,
    user_not_covered = user_not_covered,
    # dist_indic = dist_indic,
    n_added = N,
    # n_solutions = n_solutions,
    A = A,
    user_id = user_id_list,
    lp_solution = lp_solution,
    cost_install = cost_install,
    cost_relocate = cost_relocate,
    cost_total = cost_total,
    model_call = model_call,
    sum_c_mi = sum_c_mi,
    m_vec = m_vec
)

# return(x)

model_result <- extract_mc_results_relocation(x)

return(model_result)

} # end function
