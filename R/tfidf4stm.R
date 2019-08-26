#' Tf-IDf for stm DTM
#'
#'
#' @param stm an object of class simple_triplet_matrix representing a
#'     DTM, in particular, with documents as row
#' @param normalize (lgl) if TRUE (default) the weights are normalized
#'     wrt documents, i.e. row_sum equals 1 for every row.
#'
#' @return a `DocumentTermMatrix`, `simple_triplet_matrix`
#' @export
tfidf4stm <- function(stm, normalize = TRUE){
    names(stm[['dimnames']]) <- c('Docs', 'Terms')
    class(stm)               <- c(
        'DocumentTermMatrix',
        'simple_triplet_matrix'
    )

    ## Useful constants
    #
    ds <- as.integer(slam::row_sums(stm))
    ts <- as.integer(slam::col_sums(stm != 0))

    ## TF
    #
    stm[['v']] <- stm[['v']]/ds[stm[['i']]]

    ## iDF
    #
    lnts <- log2(stm[['nrow']] / (1 + ts))

    ## TF-iDF
    #
    stm <- slam:::t.simple_triplet_matrix(
        slam:::t.simple_triplet_matrix(stm) * lnts
    )

    ## Define attributes
    #
    class(stm) <- c('DocumentTermMatrix', 'simple_triplet_matrix')
    attr(stm, "weighting") <- c('term frequency - inverse document frequency (normalized)', 'tf-idf')

    stm
}
