#' Get parameters from glmmADMB models
#'
#' This function returns a list of model parameters from
#' models created with glmmADMB.
#' @param mod a model fitted with glmmADMB
#' @export
#' @examples
#' require(glmmadmb)
#' model <- glmmadmb(extortions ~ 1, data = testdata, family = "poisson")
#'
#' get_glmmadmb(model)


get_glmmadmb <- function(mod)
{
    if("glmmADMB"%in% rownames(installed.packages()) == FALSE)
        warning("This function only works with glmmadmb objects;
                it appears you do not have this package installed.")

    if(class(mod) != "glmmadmb")
        stop(paste0(deparse(substitute(mod)),
                    " is not a glmmadmb object."))

    alpha <- 1/mod$alpha

    aic <- AIC(mod)
    bic <- BIC(mod)
    llik <- logLik(mod)

    results <- list(model = mod$formula, logLik =  as.numeric(llik),
                    df = attr(llik, "df") ,AIC = aic, BIC = bic, alpha = alpha)

    if(!is.null(mod$random))
    {
        var_j <- as.numeric(mod$S)
        results$var_j <- var_j

        ICC <- var_j/(var_j + alpha)
        results$ICC <- ICC
    }
    return(results)
}
