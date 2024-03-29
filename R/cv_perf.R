#' Cross-Validation performance
#'
#' \code{cv_perf} compute the required performance for Cross-validated
#' binary classification using by the ROCR package
#'
#' @param preds (list) of object of class prediction
#' @param statistics (list, default = list(acc = 'acc')) named list of
#'     statistics of interest (using the convention of the package ROCR)
#' @param dgt (num) number of significant digits to use for the
#'     performances
#'
#' @return a list of performance
#' @export
cv_perf <- function(preds, statistics = list(acc = 'acc'), dgt = 4) {
    cv_stat <- purrr::map(preds,
        function(pred) {
            purrr::map_dbl(statistics,
                ~ ROCR::performance(pred, measure = .)@y.values[[1]][2] %>%
                  round(dgt)
            )
        }
    )

    cv_stat[['CV']] <- purrr::map_dbl(
        stats::setNames(names(statistics), names(statistics)),
        ~ purrr::transpose(cv_stat)[[.]] %>%
            unlist() %>%
            mean() %>%
            round(dgt)
    )
    cv_stat
}
