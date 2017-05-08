#' List of contingency tables
#'
#' A convenience function to output a list of contingency tables
#' from a list of chi-square tests, as those created by
#' \code{\link{batch_chisq}}.
#' @param x A list of chi-squared tests.
#' @param option Allows specifying the type of continency table
#'     to be produced. The options are:
#'     - \code{observed} counts, default option.
#'     - \code{expected} counts.
#'     - \code{ratio} of observed to expected counts.
#'     - \code{percent} row percentages.
#' @param print_option Allows specifying the output format, using
#'     the \code{\link{kable}} from package \code{\link{knitr}}. The
#'     default option \code{none} produces a list of matrix objects.
#' @keywords chi-squared, contingency table
#' @export
#' @examples
#' b <- batch_chisq(df = testdata, DV = "extortion_victim",
#'             IV = c("bribe_victim", "size"))
#'
#' chisq_list(b, option = "ratio", print_option = "pandoc")


chisq_list <- function(x, option = c("observed", "expected", "ratio",
                        "percent"), print_option = c("none",
                         "markdown", "pandoc", "latex", "html"))
{
    obs <- lapply(x, function(x) as.matrix(get("observed", x)))
    exp <- lapply(x, function(x) as.matrix(get("expected", x)))

    if(option[1] == "observed")
    {
        results <- obs
        method <- "Observed counts"
    }

    if(option[1] == "expected")
    {
        if(print_option[1] != "none")
        {
            stop("Error: Print options not available for 'expected'.")
        } else
        {
            results <- exp
        }
    }

    if(option[1] == "ratio")
    {
        results <- mapply('/', obs, exp, SIMPLIFY = FALSE)
        method <- "Ratio of observed to expected counts"
    }

    if(option[1] == "percent")
    {
        results <- lapply(obs, function(x) round(prop.table(x, 1)*100, 2))
        method <- "Row percentages"
    }

    if(print_option[1] %in% c("markdown", "pandoc", "latex", "html"))
    {
        legend <- " of (rows) "
        legend <- paste(method, legend, sep="")
        varnames <- lapply(results, function(x)
                            names(attr(x, which = "dimnames")))
        varnames <- lapply(varnames, paste, collapse = " vs. (cols) ")
        caps <- paste(legend, varnames, sep = "")

        results <- mapply(function(x, y)
        {
            knitr::kable(x, format=print_option[1], caption = y, digits = 3)
        }, results, caps)
    }
    if(!print_option[1] %in% c("none", "markdown", "pandoc", "latex", "html"))
    {
        stop("Printing method does not exist.")
    }
    return(results)
}
