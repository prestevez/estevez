#' Batch Chi-square Tests
#'
#' A wrapper for \code{\link{chisq_wrap}} to enable batch processing
#' of Chi-square tests of association between one DV and a vector of
#' at least 1 IV.
#'
#' @param df The data frame that contains the \code{IV} and \code{DV}
#' @param DV A column name containing a factor variable, will be the columns
#'     of the chi-sq contingecy tables.
#' @param IV A vector of column names of \code{length(IV) >= 1} containing
#'     factor variables.
#' @param ... pases options to the child functions, e.g. the number of
#'     replicates to use in simulation \code{B = 1000}.
#' @keywords chi-squared
#' @export
#' @examples
#' batch_chisq(df = enve_test, DV = "extortion_victim",
#'             IV = c("bribe_victim", "size"))

batch_chisq <- function(df, DV, IV, ...)
{
    if(class(df[,DV]) != "factor") stop("ERROR: DV is not a factor")

    results <- list()

    for(i in 1:length(IV))
    {
        if(class(df[,IV[i]]) != "factor") stop("ERROR: IV is not a factor")

        freqtable <- ftable(df[,IV[i]],  df[,DV])

        names(attr(freqtable, which = "col.vars")) <- DV
        names(attr(freqtable, which = "row.vars")) <- IV[i]

        chsqtst <- chisq_wrap(freqtable, ...)

        results[[IV[i]]] <- chsqtst

    }
    return(results)
}
