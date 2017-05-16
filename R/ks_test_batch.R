#' KS-Test batch
#'
#' A helper function to process batch index of Kolmogorov-Smirnov Tests,
#' default option to print results as a matrix, can print as a kintr table
#' @param x a vector of column names of counts in data
#' @param data a data frame
#' @param print_options Provides the option to print out results as
#'     a table formatted by \code{\link{kable}} from the \code{knitr} package.
#' @export
#' @examples
#' ks_test_batch(c("extortions", "bribes") data = testdata,
#'                 print_option = "pandoc")

ks_test_batch <- function(x, data,  print_option = c("none",
                          "markdown", "pandoc", "latex", "html"), ...)
{
    results_table <- sapply(x, my_ks_test, data = data, ...)

    results_table <- data.frame(t(results_table))

    new_results_table <- data.frame("KS-Statistic" =
                                        unlist(results_table$statistic))
    new_results_table$`p-value` <- unlist(results_table$p.value)

    results_table <- new_results_table

    rownames(results_table) <- x

    if(print_option[1] %in% c("markdown", "pandoc", "latex", "html"))
    {
        legend <- "Kolmogorov-Smirnov Tests"

        results_table$stars <- add_stars(results_table$`p-value`)

        results <- knitr::kable(results_table, format = print_option[1],
                                caption = legend, digits = 3)
    } else {results <- results_table}

    if(!print_option[1] %in% c("none", "markdown", "pandoc", "latex", "html"))
    {stop("Printing method does not exist.")}

    return(results)
}
