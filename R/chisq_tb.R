#' Table of chi-square tests results
#'
#' Generates a table summarising the results from a list of
#' \code{chisq.test} objects.
#' @param x A list of \code{chisq.test} objects, as the one generated
#'     by \code{batch_chisq}.
#' @param stars Option to add a column of stars indicating significance
#'     using the \code{\link{add_stars}} function. Can pass a different
#'     alpha vector of three thresholds using \code{...}.
#' @keywords chi-squared test, table
#' @export
#' @examples
#' b <- batch_chisq(testdata, DV = "extortion_victim",
#'                   IV = c("bribe_victim", "size"))
#' chisq_tb(b)


chisq_tb <- function(x, stars = TRUE, ...)
{
    Chisq <- lapply(x, function(y) y$statistic)
    df <- lapply(x, function(y) y$parameter)
    p.value <- lapply(x, function(y) y$p.value)
    CV <- lapply(x, cramersv)

    results <- as.data.frame(cbind("Chi-sq" = Chisq,
                                   "Cramer's V" = CV, df, p.value))

    if(stars == TRUE)
    {
        results$stars <- add_stars(results$p.value, ...)
    }

    return(results)
}

