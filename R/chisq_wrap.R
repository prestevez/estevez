#' Chi-square wrapper
#'
#' This function is wraps the \code{\link{chisq.test}} function
#'  from the \code{stats} package to automatically simulate the
#'  p-value if the data provided does not comply with the assumptions
#'  of the Chi-square test, specifically that there is at least
#'  one cell with less than 5 expected counts.
#'  @param x a numeric vector or matrix. x and y can also both be factors.
#'  @param y a numeric vector; ignored if x is a matrix. If x is a factor,
#'      y should be a factor of the same length.
#'  @param ... passes options to the function
#'  @keywords chi-square
#'  @export
#'  @examples
#'  chisq_wrap(x, y)
#'
#'  # x is a matrix or an object that can be coerced into a matrix
#'  chisq_wrap(x)
#'
#'  # Pass number of replicates to be used if simulation is required
#'  chisq_wrap(x, B = 1000)

chisq_wrap <- function(x, y = NULL, ...)
{
    tc <- tryCatch(chisq.test(x, y = NULL), warning = function(x) x)

    simul <- FALSE

    if(is(tc, "warning"))
    {
        simul <- TRUE
    }

    test <- chisq.test(x, y = NULL, simulate.p.value = simul, ...)

    return(test)
}

#DNAME <- deparse(substitute(x))

