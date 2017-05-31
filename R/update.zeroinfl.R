#' Update method for zeroinfl
#'
#' A method for update function that works with zeroinfl models
#' from the pscl package. The code for this method was taken from
#' \url{https://stackoverflow.com/questions/21247078/updating-a-zeroinfl-model-in-r}.
#' @param object zeroinfl model
#' @param new a formula representing the updated model
#' @export
#'
#' @examples
#' require(pscl)
#' model <- zeroinfl(extortions ~ bribes | bribes, dist = "negbin", data = testdata)
#'
#' update(model, . ~ . | . + years)
#'

update.zeroinfl <- function(object, new, ...)
{
    call <- object$call
    call$formula <- update(Formula::as.Formula(formula(object)), new)
    eval.parent(call)
}
