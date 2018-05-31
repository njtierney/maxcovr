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
#' @param solver character default is glpk, but you can also use lpSolve but currently in development is a Gurobi solver, see issue #25 : \url{https://github.com/njtierney/maxcovr/issues/25}
#' @param cost_install integer the cost of installing a new facility
#' @param cost_removal integer the cost of removing a facility
#' @param cost_total integer the total cost allocated to the project
#' @param return_early logical TRUE if I do not want to run the extraction process, FALSE if I want to just return the lpsolve model etc.
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
#'                                         cost_removal = 200,
#'                                         cost_total = 600000)
#'
#' mc_relocate
#'
#' summary(mc_relocate)
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
                                    solver = "glpk",
                                    return_early = FALSE){

# the A matrix that I feed here will be the combination of the
# existing AED locations and the potential AED locations.
# # #
    # library(tidyverse)
    # york_selected <- york %>% filter(grade == "I")
    #
    # york_unselected <- york %>% filter(grade != "I")
    # existing_facility = york_selected
    # proposed_facility = york_unselected
    # user = york_crime
    # distance_cutoff = 100
    # cost_install = 2500
    # cost_removal = 700
    # cost_total = 25000
    # solver = "gurobi"
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
# one column per variable - this is another line to optimise with c++
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


if(solver == "lpSolve"){
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
    cost_removal = cost_removal,
    cost_total = cost_total,
    model_call = model_call,
    sum_c_mi = sum_c_mi,
    m_vec = m_vec
)

# return(x)

# add a return early statement for debugging.

if(return_early){
    return(x)
} else if(!return_early){

model_result <- extract_mc_results_relocation(x)

return(model_result)

}


} else if (solver == "glpk") {

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
        cost_install = cost_install,
        cost_removal = cost_removal,
        cost_total = cost_total,
        model_call = model_call,
        sum_c_mi = sum_c_mi,
        m_vec = m_vec,
        # n_solutions = 1,
        A = A,
        user_id = user_id_list,
        glpk_solution = glpk_solution
    )

# return(x)

# THIS IS WHERE THE BUG IS - I NEED TO FIX THE EXTRACTION PART OF THE CODE
    # FOR GLPK
model_result <- extract_mc_results_relocation(x)

return(model_result)

} else if (solver == "gurobi") {

    if (!requireNamespace("gurobi", quietly = TRUE)) {
        stop("Make sure that you have installed the Gurobi software and accompanying Gurobi R package, more details at https://www.gurobi.com/documentation/7.0/refman/r_api_overview.html")

    }

    # gurobi is a little precious and doesn't take `==`.
    # constraint_directions_gurobi <- c(rep("<=", Nx), "=")
    model_call <- match.call()
    # model <- list()
    gurobi_solution <- gurobi::gurobi(model = list(
        A = constraint_matrix,
        obj = c,
        sense = constraint_directions,
        rhs = rhs_matrix,
        vtype = "B",
        modelsense = "max"
    ))

    x <- list(
        # #add the variables that were used here to get more info
        existing_facility = existing_facility,
        proposed_facility = proposed_facility,
        distance_cutoff = distance_cutoff,
        existing_user = user,
        user_not_covered = user_not_covered,
        # dist_indic = dist_indic,
        cost_install = cost_install,
        cost_removal = cost_removal,
        cost_total = cost_total,
        model_call = model_call,
        sum_c_mi = sum_c_mi,
        m_vec = m_vec,
        # n_solutions = 1,
        A = A,
        user_id = user_id_list,
        solution = gurobi_solution
    )

    J <- nrow(x$A)
    I <- ncol(x$A)

    # how many are in existence?
    n_existing <- nrow(x$existing_facility)

    # how many are proposed?
    n_proposed <- nrow(x$proposed_facility)

    # total number of facilities?
    n_facilities <- (nrow(x$existing_facility) + nrow(x$proposed_facility))

    # how many of the existing ones were removed?
    n_existing_removed <- sum(x$solution$x[1:n_existing] == 0)

    # how many additional proposed ones were selected?
    n_existing_1 <- n_existing + 1

    # how many of the proposed were chosen?
    n_proposed_chosen <- sum(x$solution$x[n_existing_1:n_facilities])

    # # how many from the existing locations were moved?
    # n_existing_removed <- n_existing - n_existing_selected

    # which ones were moved?
    which_existing_removed <-
        x$existing_facility[which(x$solution$x[1:n_existing] == 0),]

    # maybe there is now total coverage?
    # create bits to get the right vector size out for the users affected
    n_bit_3 <- n_existing + n_proposed + 1

    n_bit_4 <- length(x$solution$x)

    # number of users affected
    n_users_affected <- sum(x$solution$x[n_bit_3:n_bit_4])

    # key pieces of information
    # n_proposed_chosen
    # n_existing_removed
    # which_existing_removed

    # back to old summary method ---------------------------------------------------

    # ... surprisingly (!) this works with very few changes  good job, past nick!

    # which AEDs are to be used
    facility_solution <- x$solution$x[1:I]

    facility_id <- readr::parse_number(colnames(x$A))

    # user_id <- x$user_id

    # which facilities are selected?
    facility_temp <- tibble::tibble(
        # get the facility ids
        facility_id = facility_id,
        facility_chosen = facility_solution) %>%
        dplyr::filter(facility_chosen == 1)

    facility_selected <- dplyr::bind_rows(x$existing_facility,
                                          x$proposed_facility) %>%
        dplyr::mutate(facility_id = facility_id) %>%
        dplyr::filter(facility_id %in% facility_temp$facility_id) %>%
        # drop facility_id as it is not needed anymore
        dplyr::select(-facility_id)

    # which OHCAs are affected
    user_solution <- x$solution$x[c(I + 1):c(I + J)]

    user_temp <- tibble::tibble(
        user_id = x$user_id,
        user_chosen = user_solution) %>%
        dplyr::filter(user_chosen == 1)

    user_affected <- dplyr::left_join(user_temp,
                                      # x$user_not_covered,
                                      x$existing_user,
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

    dist_sum_df <- maxcovr::nearest_facility_dist(facility = facility_sum_prep,
                                                  user = user_sum_prep) %>%
        dplyr::as_data_frame() %>%
        dplyr::rename(user_id = V1,
                      facility_id = V2,
                      distance = V3) %>%
        dplyr::mutate(is_covered = (distance <= x$distance_cutoff))

    model_coverage <-  dist_sum_df %>%
        dplyr::summarise(
            total_cost = as.numeric(x$cost_total),
            install_cost = as.numeric(x$cost_install),
            cost_removal = as.numeric(x$cost_removal),
            n_proposed_chosen = n_proposed_chosen,
            n_existing_removed = n_existing_removed,
            # n_added = as.numeric(x$n_added),
            distance_within = as.numeric(x$distance_cutoff),
            n_cov = sum(is_covered),
            pct_cov = (sum(is_covered) / nrow(.)),
            # really, these are not needed
            n_not_cov =  (sum(is_covered == 0)),
            pct_not_cov = (sum(is_covered == 0) / nrow(.)),
            dist_avg = mean(distance),
            dist_sd = stats::sd(distance))

    # add the original coverage
    existing_coverage <- x$existing_facility %>%
        nearest(x$existing_user) %>%
        dplyr::mutate(is_covered = (distance <= x$distance_cutoff)) %>%
        dplyr::summarise(
            # n_added = 0,
                         distance_within = as.numeric(x$distance_cutoff),
                         n_cov = sum(is_covered),
                         pct_cov = (sum(is_covered) / nrow(.)),
                         n_not_cov =  (sum(is_covered == 0)),
                         pct_not_cov = (sum(is_covered == 0) / nrow(.)),
                         dist_avg = mean(distance),
                         dist_sd = stats::sd(distance))

    # add a summary coverage
    summary_coverage <- dplyr::bind_rows(existing_coverage,
                                         model_coverage)

    # which proposed facilities had a facility installed?
    is_installed_prep <- x$solution$x[n_existing_1:n_facilities]

    # update proposed facilities with this info
    x$proposed_facility <- x$proposed_facility %>%
        dplyr::mutate(is_installed = is_installed_prep)

    # which existing facilities were relocated?
    is_relocated <- !x$solution$x[1:n_existing]

    # update existing facilities with information about relocation
    x$existing_facility <- x$existing_facility %>%
        dplyr::mutate(is_relocated = is_relocated)

    # which users were affected?
    is_covered <- x$solution$x[n_bit_3:n_bit_4]

    # update users with information about relocation
    x$existing_user <- x$existing_user %>%
        dplyr::mutate(is_covered = is_covered)

    res <- tibble::tibble(

        # augmented information about each incoming dataframe
        user = list(x$existing_user),
        existing_facility = list(x$existing_facility),
        proposed_facility = list(x$proposed_facility),

        # basically existing_facility and proposed facility
        facilities_selected = list(facility_selected),

        # simple summary info
        model_coverage = list(model_coverage),
        existing_coverage = list(existing_coverage),
        summary = list(summary_coverage),

        # model_call stuff
        solution_vector = list(x$solution$x),
        total_cost = list(x$cost_total),
        distance_cutoff = list(x$distance_cutoff),
        solver_used = list(solver),
        model_call = list(x$model_call)

        # # things to recover full data - no longer needed
        # is_covered = list(is_covered),
        # is_installed = list(is_installed),
        # is_relocated = list(is_relocated),

        # # data input
        # user = list(user),
        # proposed_facility = list(proposed_facility),
        # existing_facility = list(existing_facility)

        # #unlikely to need again
        # Pretty sure this can be retrieved from the above information.
        # user_affected = list(user_affected),
        # which_existing_removed = list(which_existing_removed),

    )
    # not really sure if I need to provide the user + facility solution
    # but perhaps I could provide this in another function to extract
    # the working parts of the optimisation
    # user_solution = user_solution,
    # facility_solution = facility_solution,
    # facilities_users_merge = facilities_users_merge,
    #add the variables that were used here to get more info

    # res <- c(class(res),"maxcovr_relocation")
    class(res) <- c("maxcovr_relocation",class(res))
    # class(res) <- c("maxcovr_relocation")

    return(res)

#
#
#
#
#
#     return(x)

}


} # end function
