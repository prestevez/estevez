#' Vicimisation distribution table
#'

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
