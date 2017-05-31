#' compare a glmmadmb hurdle to non-hurdle models
#'
#' This function compares the joint parameters of a hurdle model
#' specified by two distinct models (e.g. a binary and a truncated
#' count model) with the parameters of a non-hurdle count model.
#' No checks are made to ensure that the models supplied are comparable.
#' The function is designed for glmmADMB models only.
#' @param mlogit a binary model
#' @param mcount a truncated count model
#' @param ... a series of at least one comparable glmmadmb model
#' @param modnames a vector of names  for the models supplied. \code{mlogit}
#' and \code{mcount} correspond to "Hurdle", while the rest of the models
#' supplied in \code{...} correspond to the rest of the names. If only one
#' model is supplied in \code{...}, only the first two names are used.
#' @export

compare_hurdle2other <- function(mlogit, mcount, ...,
                                 modnames = c("Hurdle", "Negbin.", "ZINB"))
{
    hurdle_params <- joint_ic(mlogit, mcount)
    results_h <- t(data.frame(hurdle_params))

    othermods <- list(...)

    lparams <- lapply(othermods, function(x)
    {
        g <- get_glmmadmb(x)
        df <- t(data.frame(g[2:5]))
        return(df)
    })

    results_o <- lparams[[1]]
    if(length(othermods) > 1)
    {
        for(i in 2:length(othermods))
        {
            results_o <- cbind(results_o, lparams[[i]])
        }
    }

    else
    {
        modnames <- modnames[1:2]
    }

    results <- cbind(results_h, results_o)
    colnames(results) <- modnames

    return(results)
}
