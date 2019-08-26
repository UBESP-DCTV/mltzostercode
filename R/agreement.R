#' Agreement between models
#'
#' @param mod1 a (list) of models trained using the same algorithm
#' @param mod2 a (list) of models trained using the same (different from
#'     `mod1`) algoritm
#' @param ref (int) vector of correct classes
#'
#' @return a (list) with agreement metrics
#' @export
agreement <- function(mod1, mod2, ref) {
    if (is.matrix(mod1[[1]])) {
        mod1 <- map(mod1, ~ .[, 1])
    }
    if (is.matrix(mod2[[1]])) {
        mod2 <- map(mod2, ~ .[, 1])
    }
    res1 <- unlist(mod1)
    res2 <- unlist(mod2)
    message(str(res1))
    message(str(res2))

    list(
        disagree    = sum(res1 != res2),
        true_agree  = sum((res1 == res2) & (res1 == ref)),
        false_agree = sum((res1 == res2) & (res1 != ref)),
        cohen_cappa = psych::cohen.kappa(data.frame(res1, res2))$confid[1, ]
    )
}
