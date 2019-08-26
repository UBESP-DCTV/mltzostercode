#' Cross-Validation performance
#'
#' \code{cv_perf} compute the required performance for Cross-validated
#' binary classification using by the ROCR package
#'
#' @param preds (list) of object of class prediction
#' @param statistics (list, default = list(acc = 'acc')) named list of statistics
#' of interest (using the convention of the package ROCR)
#'
#' @return
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

    cv_stat[['CV']] <- map_dbl(setNames(names(statistics), names(statistics)),
        ~ transpose(cv_stat)[[.]] %>% unlist() %>% mean() %>% round(dgt)
    )
    cv_stat
}
