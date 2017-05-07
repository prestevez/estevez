#' Cramer's V
#'
#' This function calculates Cramer's V from an abject created by the
#' \code{\link{chisq.test}} function from the \code{stats} package.
#' @param x An abject created by \code{\link{chisq.test}}.
#' @keywords frequency tables, measures of association, chi-square test
#' @export
#' @examples
#' cv_test(x)

cv_test = function(x)
{
    CV = sqrt(x$statistic /
                  (sum(x$observed) * (min(ncol(x$observed),
                                          nrow(x$observed)) - 1)))

    CV <- as.numeric(CV)
    names(CV) <- "Cramer's V"

    return(CV)
}

