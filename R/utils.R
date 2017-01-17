
#' @export
print.maxcovr_relocation <- function(x){

    cat("-----------------------------------------------" ,
        "\nModel Fit: maxcovr relocation model",
        "\n-----------------------------------------------",
        "\nTotal Cost:       ",
        paste(deparse(x$model_coverage[[1]]$total_cost)),
        "\nInstallation Cost:",
        paste(deparse(x$model_coverage[[1]]$install_cost)),
        "\nRelocation Cost:  ",
        paste(deparse(x$model_coverage[[1]]$cost_relocate)),
        "\nDistance Threshold:",
        paste(deparse(x$model_coverage[[1]]$distance_within)),
        "\nExisting Facilities Removed:",
        paste(deparse(x$model_coverage[[1]]$n_existing_removed)),
        "\nProposed Facilities Selected:",
        paste(deparse(x$model_coverage[[1]]$n_proposed_chosen)),
        "\nUsers Covered (Additional):",
        sprintf("%s (%s)",
                x$model_coverage[[1]]$n_cov,
                x$model_coverage[[1]]$n_cov -
                    x$existing_coverage[[1]]$n_cov),
        "\nProportion Covered (Additional):",
        sprintf("%s (%s)",
                round(x$model_coverage[[1]]$pct_cov,4),
                round(
                    (x$model_coverage[[1]]$pct_cov -
                         x$existing_coverage[[1]]$pct_cov),
                    4)
                ),
        "\n-----------------------------------------------"
    )

}

#' @export
is.maxcovr_relocation <- function(x) {

    inherits(x, "maxcovr_relocation")

}
