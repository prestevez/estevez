#' Index of dispersion for count data
#'
#' Takes a vector and calculates the index of dispersion
#' Then evaluates its significance for Overdispersion only
#' Returns a list with named numbers
#' @param x a vector of counts or the name of a column of counts in data
#' @param data a data frame
#' @export
#' @examples
#' dispersion_index("extortions", data = testdata)
#'
#' lapply(c("extortions, "bribes), dispersion_index, data = testdata)

dispersion_index <- function(x, data = NULL)
{
    if(is.data.frame(data)) {xvar <- data[,x]; xname <- x }
    else {xvar <- x; xname <- deparse(substitute(x))}

    if(!class(xvar) %in% c("numeric", "integer"))
    {stop("Variable is not numeric.")}

    mu <- mean(xvar, na.rm=TRUE)
    names(mu) <- "Mean"
    v <- var(xvar, na.rm=TRUE)
    names(v) <- "Variance"
    n <- length(xvar)
    df <- n-1
    names(df) <- "df"
    index <- ((df)*v)/mu
    names(index) <- "I"
    pval <- pchisq(index, df, lower.tail=FALSE)
    names(pval) <- "p-value"
    pv95 <- qchisq(.95, df)
    names(pv95) <- "95% Chi-sq"

    return(c(index, pval, df, pv95))
}
