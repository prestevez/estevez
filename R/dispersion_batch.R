#' Dispersion index batch function
#'
#' A helper function to process batch index of dispersion tests and
#' add names of the variables to the results, default option to print
#' results as a matrix, can print as a kintr table
#' @param x a vector of column names of counts in data
#' @param data a data frame
#' @param print_options Provides the option to print out results as
#'     a table formatted by \code{\link{kable}} from the \code{knitr} package.
#' @export
#' @examples
#' dispersion_batch(x = c("extortions", "bribes"), data = testdata)
#' dispersion_batch(x = c("extortions", "bribes"), data = testdata,
#'                   print = "pandoc")

dispersion_batch <- function(x, data,  print_option = c("none",
                        "markdown", "pandoc", "latex", "html"))
{
    results_list <- lapply(x, dispersion_index, data = data)
    names(results_list) <- x

    results_table <- t(simplify2array(results_list))

    if(print_option[1] %in% c("markdown", "pandoc", "latex", "html"))
    {
        legend <- "Index of dispersion tests"

        results_table <- as.data.frame(results_table)

        results_table$stars <- add_stars(results_table[,5])

        results <- knitr::kable(results_table, format = print_option[1],
                                caption = legend, digits = 3)
    } else {results <- results_table}

    if(!print_option[1] %in% c("none", "markdown", "pandoc", "latex", "html"))
    {stop("Printing method does not exist.")}

    return(results)
}
