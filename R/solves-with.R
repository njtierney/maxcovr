solve_with_lpsolve <- function(objective_in,
                               const_mat,
                               const_dir,
                               const_rhs,
                               existing_facility,
                               proposed_facility,
                               distance_cutoff,
                               existing_user,
                               user_not_covered,
                               n_added,
                               A,
                               user_id,
                               model_call){

    lp_solution <- lpSolve::lp(
        direction = "max",
        objective.in = objective_in,
        const.mat = const_mat,
        const.dir = const_dir,
        const.rhs = const_rhs,
        transpose.constraints = TRUE,
        all.bin = TRUE,
        num.bin.solns = 1,
        use.rw = TRUE
    )

    # remove the constraints, as they are too big
    lp_solution[["constraints"]] <- NULL

    x <- list(
        existing_facility = existing_facility,
        proposed_facility = proposed_facility,
        distance_cutoff = distance_cutoff,
        existing_user = existing_user,
        user_not_covered = user_not_covered,
        n_added = n_added,
        A = A,
        user_id = user_id,
        solution = lp_solution,
        model_call = model_call
    )

    model_result <- extract_mc_results(x)

    return(model_result)

}
