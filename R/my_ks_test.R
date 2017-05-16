#' My Kolmogorov-Smirnov Tests
#'
#' Performs two-sample Kolmogorov-Smirnov tests based on the
#' \code{\link{dgof::ks.test}} from the \code{\link{dgof}} package
#' and the Monte Carlo replicates from the \code{\link{mc_gini_test}}
#' @param x a vector of counts or the name of a column of counts in data
#' @param data a data frame
#' @param simulate.p.value logical to indicate if KS-test p-value should be
#'     calculated using Monte Carlo simulation
#' @param B number of replicates of the null hypothesis distribution, and of
#'     the KS-test p-value if simulate.p.value is \code{TRUE}
#' @param alternative Alternative hypothesis: The CDF is above the null
#' @param family the distribution of the null hypothesis
#' @export
#' @examples
#' my_ks_test(x = "extortions", data = testdata, family = "poisson")

my_ks_test <- function(x, data = NULL, simulate.p.value = TRUE, B = 2000,
                       alternative = "greater",
                       family = c("poisson", "nbinom"))
{
    if(is.data.frame(data)) {xvar <- data[,x]; xname <- x }
    else {xvar <- x; xname <- deparse(substitute(x))}

    if(!class(xvar) %in% c("numeric", "integer"))
    {stop("Variable is not numeric.")}

    gini_test <- mc_gini_test(x, data = data, family = family[1],
                                  keep_reps = TRUE, plots = FALSE, reps = B)
    all_expected <- unlist(gini_test$keep_reps)

    results <- dgof::ks.test(xvar, y = ecdf(all_expected),
                             sumilate.p.value = simulate.p.value,
                             B = B, alternative = alternative)
    results$data.name <- xname

    return(results)

}
