#' Confidence interval for performance
#'
#' @param perf (list) of statistics for a model (e.g. CV)
#' @param alpha (num) level of significance for the confidence interval
#'
#' @return a data frame with the confidence interval for the model
#' @export
ci <- function(perf, alpha = .05) {
    p <- 1 - (alpha / 2)
    df <- length(perf) - 2
    delta <- qt(p = p, df = df)

    s <- transpose(perf[-length(perf)]) %>% map(~ unlist(.)) %>% map_dbl(~ sd(.))
    res <- perf$CV + ((s %*% t(c(-1, 1)) * delta) / sqrt(length(perf) - 1))
    colnames(res) <- c('ci95inf', 'ci95sup')
    data.frame(res, cv_avr = perf[['CV']])
}
