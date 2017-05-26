#' Auto releviling function for factors of count data
#'
#' A helper function that turns a vector of count into factor data.
#' If there are more than 5 levels in the factor, it aggregates those
#' above the fifth level to a single level.
#' @param x a numeric or integer vector.
#' @param max_levels offers the option to control the minimum
#'     numer of levels in the new factor
#' @keywords factor, as.factor, count data
#' @export
#' @examples
#' fac_relevel(testdata$extortions)

fac_relevel <- function(x, min_levels = 6)
{
    if(!class(x) %in% c("numeric", "integer"))
    {stop("x is not numeric or integer")}

    fac <- factor(x)

    if(nlevels(fac) > min_levels)
    {
        levels(fac)[(min_levels):nlevels(fac)] <-
            rep(paste0(min_levels-1, "+"), nlevels(fac) - (min_levels-1))
    }

    return(fac)
}
