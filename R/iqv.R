#' Index of Qualitative Variation
#'
#' A simple index of qualitative variation based on the equation in
#' \url{http://bit.ly/2qThZgd}. The equation is:
#' \deqn{IQV = \frac{k(100^2 - \sum p^2)}{100^2(k-1)}}
#' @param x a dataframe with columns of counts for each k category
#' @export
#' @examples
#' years_by_state <- reshape2::dcast(testdata, CVE_ENT ~ yearsquant)
#' iqv(years_by_state)
#'
#' # IQV per state
#' apply(years_by_state, 1, iqv)

iqv <- function(x)
{
    k <- ncol(x)
    if(is.null(k)) {k <- length(x)}
    tot<- sum(x)
    vartots <- sapply(x, sum)
    perc <- (vartots/tot)*100

    result <- (k * (100^2 - sum(perc^2))) / (100^2 * (k - 1))

    return(result*100)
}
