#' Print a list of kable tables
#'
#' Prints the kables direct to output, facilitating pickup by pandoc.
#' @param x a list of kable tables
#' @export
#' @examples
#' b <- batch_chisq(df = testdata, DV = "extortion_victim",
#'             IV = c("bribe_victim", "size"))
#'
#' l <- chisq_list(b, option = "ratio", print_option = "pandoc")
#'
#' print_chisq_l(l)

print_kables <- function(x)
{
    if(!is.list(x)) {stop(past0(x, " is not a list."))}

    for(i in 1:length(x)) print(x[[i]])
}
