#' x_data_deparser function
#'
#' A helper function used inside other functions to be able
#' to call either x as an object or as a column name of data
#' @param x a vector or a name of a column in \code{data}
#' @param data a data frame.
#' @export

x_data_deparser <- function(x, data = NULL)
{
    if(is.data.frame(data)) {xvar <- data[,x]; xname <- x }
    else {xvar <- x; xname <- deparse(substitute(x))}
    return(list(xvar, xname))
}
