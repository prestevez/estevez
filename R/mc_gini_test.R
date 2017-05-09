#' Monte Carlo Gini Index Test
#'
#' Takes a vector of counts and calculates its Gini coefficient
#' then it generates a Monte Carlo distribution of Gini coefficients
#' under the specified family (Poisson or nbinom) and compares the
#' observed Gini with the MC confidence interval.
#' There is an option to generate a plot of the values and critical region.
#' @param x A numeric or intenger vector of event counts. If
#'     \code{data} is provided, x is the character label of the column
#'     of event counts.
#' @param data A data frame.
#' @param reps The number of Monte Carlo replicates
#' @param keep_reps Logical option to keep the vector of Gini Index statistics
#'     for the Monte Carlo replications.
#' @param family Family distribution for the Null Hypothesis.
#' @param plots Logical indicating whether to generate a plot of the test.
#' @keywords Monte Carlo, simulation, poisson, negative binomial, inequality,
#'     Gini Index
#' @export
#' @examples
#' mc_gini_test("extortions", data = testdata, plots = TRUE, family = "poisson")


mc_gini_test <- function(x, data = NULL, reps = 2000, keep_reps = FALSE,
                         family = c("poisson", "nbinom"), plots = FALSE)
{
    if(is.data.frame(data)) {xvar <- data[,x]; xname <- x }
    else {xvar <- x; xname <- deparse(substitute(x))}

    if(!class(xvar) %in% c("numeric", "integer"))
    {stop("Variable is not numeric.")}

    # Calculate observed Gini
    obs_gini <- ineq::ineq(xvar, type="Gini")
    names(obs_gini) <- paste("Observed Gini Coefficient for ", xname, sep = "")

    # Calculate MC distributions
    if(family[1] == "nbinom")
    {
        nb_estimates <- MASS::fitdistr(xvar, "Negative Binomial")

        mc_reps <- lapply(1:reps, function(x)
        {
            rnbinom(length(xvar), size=nb_estimates$estimate[1],
                    mu=nb_estimates$estimate[2])
        })
    }
    else
    {
        mc_reps <- lapply(1:reps, function(x){rpois(length(xvar), mean(xvar))})
    }

    # Calculate the Gini coefficients for MC distributions
    mc_gini <- unlist(lapply(mc_reps, function(x) ineq::ineq(x, type="Gini")))

    # 95% confidence interval
    mc_confint <- quantile(mc_gini, c(0.025, 0.975))
    names(mc_confint) <- c("MC 2.5%", "MC 97.5%")

    # MC mean
    mc_mean <- mean(mc_gini)
    names(mc_mean) <- "Monte Carlo mean"

    names(reps) <- "Replicates"

    # Test

    mc_test <- obs_gini < mc_confint[1] | obs_gini > mc_confint[2]
    names(mc_test) <- "Alternative Hypothesis"

    results <- list(DV = xname,
                    stat = obs_gini,
                    mc_mean = mc_mean,
                    mc_confint = mc_confint,
                    reps = reps,
                    mc_test = mc_test)

    # Plot
    if(plots==TRUE)
    {
        legend <- paste(xname, ": ", sep="")
        plottitle <- "Obs. vs. Exp. ("

        plottitle <- paste(legend, plottitle, family[1], ")", sep = "")

        plot <- ggplot2::ggplot(data.frame(gini = mc_gini),
                                ggplot2::aes(gini)) +
            ggplot2::geom_density() +
            ggplot2::geom_vline(xintercept = mc_confint, linetype="dashed") +
            ggplot2::geom_vline(xintercept = obs_gini, colour="red") +
            ggplot2::ggtitle(plottitle) +
            ggplot2::theme_bw()

        results$plot <- plot
    }

    if(keep_reps == TRUE)
    {
        results$mc_gini <- mc_gini
    }

    # Return results
    return(results)
}
