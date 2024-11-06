check_gurobi_installed <- function() {

    gurobi_url <- "https://www.gurobi.com/documentation/7.0/refman/r_api_overview.html"

    if (!requireNamespace("gurobi", quietly = TRUE)) {
        cli::cli_abort(
            message = c(
                "{.pkg gurobi} R package must be installed to use solver \\
                {.var gurobi}",
                "You can find more details at {.url {gurobi_url}}."
            )
        )

    }
}
