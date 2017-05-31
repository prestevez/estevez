#' Compare parameters from hurdle models
#'
#' Comapre parameters from at least two hurdle models supplied
#' as pairs in the \code{\link{joint_ic}} function.
#' @param ... recieves the at least two \code{\link{joint_ic}}
#'     functions with two models each (one binary and one count).
#' @param modnames option to add a character vector supplying a short
#'     descriptive name for each pair of models
#' @export

hurdle_compare <- function(... , modnames = NULL)
{
    lparams <- list(...)

    results_h <- lparams[[1]]
    for(i in 2:length(lparams))
    {
        results_h <- cbind(results_h, lparams[[i]])
    }
    colnames(results_h) <- modnames
    return(results_h)
}
