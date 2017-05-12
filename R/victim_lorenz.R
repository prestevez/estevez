#' Lorenz curves for victimisation distributions
#'
#' A wrapper for a series of functions to generate Lorenz plots from
#' vectors of counts of victimisation incidents. Includes option to include
#' Expected counts under Poisson or Negative Binomial expected counts.
#' @param x a vector of counts or the name of a column of counts in data
#' @param data a data frame
#' @param family an option based on \code{\link{gini_test}} that
#'     adds curves for the expected counts under the null hypothesis
#'     of a poisson or a negative binomial distribution using an aggregate
#'     Monte Carlo simulation with 500 replicates
#' @param reps passes option of replicates to \code{\link{gini_test}}
#' @keywords Lorenz Curve, Monte Carlo, Count Data
#' @examples
#' victim_lorenz("bribes", data = testdata, family = "poisson")
#'

victim_lorenz <- function(x, data = NULL, family = c("none", "poisson",
                                                     "nbinom"), reps = 500)
{
    if(is.data.frame(data)) {xvar <- data[,x]; xname <- x }
    else {xvar <- x; xname <- deparse(substitute(x))}

    if(!class(xvar) %in% c("numeric", "integer"))
    {stop("Variable is not numeric.")}

    v_table <- victim_table(x = x, data = data, print_option = "none")
    v_cum <- victim_cumulative(v_table)
    v_cum[is.na(v_cum)] <- 100
    v_cum$type <- "Observed"

    gg_v_cum <- reshape2::melt(v_cum[,-1], id.vars = c("Incidents", "type"),
                             variable.name = "pop",
                             value.name = "Targets")

    if(family[1] %in% c("poisson", "nbinom"))
    {
        gini_test <- mc_gini_test(x = x, data = data, family = family[1],
                                  keep_reps = TRUE, plots = FALSE, reps = reps)
        all_expected <- unlist(gini_test$keep_reps)
        expected_table <- victim_table(all_expected, print_option = "none")
        expected_cumper <- victim_cumulative(expected_table)
        expected_cumper[is.na(expected_cumper)] <- 100
        expected_cumper$type <- family[1]

        gg_expected <- reshape2::melt(expected_cumper[,-1],
                                      id.vars = c("Incidents", "type"),
                                      variable.name = "pop",
                                      value.name = "Targets")
        gg_data <- rbind(gg_v_cum, gg_expected)
    }
    else
    {
        gg_data <- gg_v_cum
    }

    p <- ggplot2::ggplot(gg_data, ggplot2::aes(Targets, Incidents, linetype = type)) +
        ggplot2::geom_line() +
        ggplot2::geom_segment(ggplot2::aes(x=1, y=1, xend=100, yend=100,
                                           linetype = "Equality")) +
        ggplot2::ggtitle(paste("Lorenz Curves: ", xname, sep = "")) +
        ggplot2::facet_wrap( ~ pop) +
        ggplot2::theme_bw() +
        ggplot2::theme(legend.title=ggplot2::element_blank(),
                       legend.position="bottom",
                       plot.title = ggplot2::element_text(hjust = 0.5))

    return(p)
}
