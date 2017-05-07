#' Add Significance Stars
#'
#' This function creates a character vector of \code{length(pval)}
#' adding significance stars according to the \code{alpha} values
#' specified.
#' @param pval A vector of pvalues
#' @param alpha a vector of three critical significance thresholds, defaults to
#'     \code{c(0.001, 0.01, 0.05)}.
#' @keywords significance, hypothesis testing, p-values
#' @export
#' @examples
#' p <- c(0.1, 0.3, 0.0005, 0.02)
#' add_stars(p)

add_stars <- function(pval, alpha = c(0.001, 0.01, 0.05))
{
    ifelse(pval < alpha[1], "***",
           ifelse(pval < alpha[2], "**",
                  ifelse(pval < alpha[3], "*", "")))
}
