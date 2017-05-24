#' Chi-sq test of a count distribution
#'
#' Performs a chi-squared test of a count distribution against
#' a vector of probabilities calculated via a monte carlo
#' simulation of distributions under the null hypothesis
#' of poisson or negative binomial expected counts.
#' @param x a vector of count data or a name of a count data column in data
#' @param data a data frame
#' @param B nuber of replicates for simulating the distributions and
#'     for calculating the p-value of the chi-squared test if any expected
#'     counts are fewer than 5.
#' @param family families for the null hypothesis
#' @export
#' @examples
#' chisq_count(testdata$extortions, family = "poisson")
#'
#' # batch using \code{\link{chisq_tb}}
#' lx <- lapply(c("extortions", "bribes"), chisq_count, testdata, B = 2000, family = "poisson")
#' chisq_tb(lx, stars = TRUE)

chisq_count <- function(x, data = NULL, B = 500,
                        family = c("poisson", "nbinom"))
{
    xvar <- x_data_deparser(x = x, data = data)[[1]]
    xname <- x_data_deparser(x = x, data = data)[[2]]

    if(!class(xvar) %in% c("numeric", "integer"))
    {stop("Variable is not numeric.")}

    fac_xvar <- fac_relevel(xvar)

    gini_test <- mc_gini_test(x, data = data, family = family[1],
                              keep_reps = TRUE, plots = FALSE, reps = B)
    all_exp <- unlist(gini_test$keep_reps)

    fac_exp <- fac_relevel(all_exp)

    if(nlevels(fac_exp) != nlevels(fac_xvar))
    {
        if(nlevels(fac_exp) > nlevels(fac_xvar))
        {
            min_levels <- nlevels(fac_xvar)
            fac_exp <- fac_relevel(all_exp, min_levels = min_levels)
        } else
        {
            min_levels <- nlevels(fac_exp)
            fac_xvar <- fac_relevel(xvar, min_levels = min_levels)
        }
    }

    exp_tab <- table(fac_exp)
    exp_probs <- exp_tab/sum(exp_tab)

    obs_tab <- table(fac_xvar, dnn = xname)

    test <- chisq_wrap(obs_tab, p = exp_probs, B=B)
    test$data.name <- paste0(xname, " vs. ", family[1])


    return(test)
}
