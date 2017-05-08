
chisq_list <- function(x, option = c("observed", "expected", "ratio",
                        "percent"), print_option = c("none",
                         "markdown", "pandoc", "latex", "html"))
{
    obs <- lapply(x, function(x) as.matrix(get("observed", x)))
    exp <- lapply(x, function(x) as.matrix(get("expected", x)))

    if(option[1] == "observed")
    {
        results <- obs
        method <- "Observed counts"
    }

    if(option[1] == "expected")
    {
        if(print_option[1] != "none")
        {
            stop("Error: Print options not available for 'expected'.")
        } else
        {
            results <- exp
        }
    }

    if(option[1] == "ratio")
    {
        results <- mapply('/', obs, exp, SIMPLIFY = FALSE)
        method <- "Ratio of observed to expected counts"
    }

    if(option[1] == "percent")
    {
        results <- lapply(obs, function(x) round(prop.table(x, 1)*100, 2))
        method <- "Row percentages"
    }

    if(print_option[1] %in% c("markdown", "pandoc", "latex", "html"))
    {
        legend <- " of (rows) "
        legend <- paste(method, legend, sep="")
        varnames <- lapply(results, function(x)
                            names(attr(x, which = "dimnames")))
        varnames <- lapply(varnames, paste, collapse = " vs. (cols) ")
        caps <- paste(legend, varnames, sep = "")

        results <- mapply(function(x, y)
        {
            knitr::kable(x, format=print_option[1], caption = y, digits = 3)
        }, results, caps)
    }
    if(!print_option[1] %in% c("none", "markdown", "pandoc", "latex", "html"))
    {
        stop("Printing method does not exist.")
    }
    return(results)
}



