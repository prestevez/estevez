#' Auto releviling function for factors of count data
#'
#' A helper function that turns a vector of count into factor data.
#' If there are more than 5 levels in the factor, it aggregates those
#' above the fifth level to a single level.
#' @param x a numeric or integer vector.
#' @keywords factor, as.factor, count data
#' @export
#' @example
#' fac_relevel(testdata$extortions)

fac_relevel <- function(x)
{
    if(!class(x) %in% c("numeric", "integer"))
    {stop("x is not numeric or integer")}

    fac <- factor(x)

    if(nlevels(fac) > 5)
    {
        levels(fac)[6:nlevels(fac)] <- rep("5+", nlevels(fac) - 5)
    }

    return(fac)
}
