#' Vicimisation distribution table
#'
#' A function to create distribution tables of victimisation counts.
#' @param x A numeric or intenger vector of event counts. If
#'     \code{data} is provided, x is the character label of the column
#'     of event counts.
#' @param data A data frame.
#' @param print_options Provides the option to print out results as
#'     a table formatted by \code{\link{kable}} from the \code{knitr} package.
#' @keywords distribution
#' @export
#' @examples
#' victim_table(testdata$extortions)
#'
#' victim_table("extortions", data = testdata, print_option = "pandoc")

victim_table <- function(x, data = NULL, print_option = c("none",
                          "markdown", "pandoc", "latex", "html"))
{
    if(is.data.frame(data)) {xvar <- data[,x]; xname <- x }
    else {xvar <- x; xname <- deparse(substitute(x))}

    if(!class(xvar) %in% c("numeric", "integer"))
        {stop("Variable is not numeric.")}

    distri <- data.frame(table(xvar))
    colnames(distri) <- c("Events", "Prevalence")

    distri$Events <- as.integer(as.character(distri$Events))
    distri$Incidence <- distri$Events * distri$Prevalence
    distri$Repeats <- distri$Incidence - distri$Prevalence

    distri$"Target_%" <- prop.table(distri$Prevalence) * 100
    distri$"Victim_%"[2:nrow(distri)] <-
        prop.table(distri[2:nrow(distri),"Prevalence"]) * 100
    distri$"Incident_%" <- prop.table(distri$Incidence) * 100

    distri[1,"Incidence"] <- NA
    distri[1:2,"Repeats"] <- NA
    distri[1,"Incident_%"] <- NA
  
    distri$"Repeats_%" <- prop.table(distri$Repeats) * 100
    distri[1:2,"Repeats_%"] <- NA

    if(print_option[1] %in% c("markdown", "pandoc", "latex", "html"))
    {
        legend <- "Victimisation distribution of "
        cap <- paste(legend, xname, ".", sep = "")

        results <- knitr::kable(distri, format = print_option[1], caption = cap,
                                digits = 3)
    } else {results <- distri}

    if(!print_option[1] %in% c("none", "markdown", "pandoc", "latex", "html"))
    {stop("Printing method does not exist.")}

    return(results)
}
