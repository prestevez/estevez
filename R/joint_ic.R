#' Joint hurdle parameters
#'
#' Produces the joint paramters of a hurdle model based on
#' two distinct models: one modeling the binary part, and the
#' second modeling the truncated count part. Calculation of the
#' params is done simply by adding the paramters of the independent
#' models as suggested by Cameron and Trivedi (2013) and Hilbe (2011).
#' No checks are made to ensure models are indeed part of a hurdle equation.
#' @param m1 A model
#' @param m2 Another model
#' @export

joint_ic <- function(m1, m2)
{
    mods <- list(m1, m2)

    joint_ls <- lapply(mods, logLik)
    dfs <- sapply(joint_ls, attr, "df")
    AICs <- sapply(mods, AIC)
    BICs <- sapply(mods, BIC)

    log_lik <- sum(unlist(joint_ls))
    df <- sum(dfs)
    aic <- sum(AICs)
    bic <- sum(BICs)

    return(list(logLik = log_lik, df = df, AIC = aic, BIC = bic))
}
