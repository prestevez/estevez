#' Vicimisation distribution table
#'

victim_table <- function(x, data = NULL, print_option = c("none","markdown",
                         "pandoc", "latex", "html"), repeats = FALSE)
{
    if(is.data.frame(data)) {xvar <- data[,x]; xname <- x }
    else {xvar <- x; xname <- deparse(substitute(x))}

    if(!class(xvar) %in% c("numeric", "integer"))
        {stop("Variable is not numeric.")}

    distri <- data.frame(table(xvar))
    colnames(distri) <- c("Events", "Prevalence")

    distri$Events <- as.integer(as.character(distri$Events))
    distri$Incidence <- distri$Events * distri$Prevalence

    distri$"Target_%" <- prop.table(distri$Prevalence) * 100
    distri$"Victim_%"[2:nrow(distri)] <-
        prop.table(distri[2:nrow(distri),"Prevalence"]) * 100
    distri$"Incident_%" <- prop.table(distri$Incidence) * 100
    distri[1,"Incident_%"] <- NA

    print(xname)
    return(distri)
}
