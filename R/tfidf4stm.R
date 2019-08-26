#' Tf-IDf for stm DTM
#'
#'
#' @param stm an object of class simple_triplet_matrix rappresenting a DTM, in
#'  particular, with documents as row
#' @param normalize (lgl) if TRUE (default) the weights are normalized wrt
#' documents, i.e. row_sum equals 1 for every row.
#'
#' @return
#' @export
#'
#' @examples
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

    ## Tf
    #
    stm[['v']] <- stm[['v']]/ds[stm[['i']]]

    ## Idf
    #
    lnts <- log2(stm[['nrow']] / (1 + ts))

    ## Tf-Idf
    #
    stm <- slam:::t.simple_triplet_matrix(
        slam:::t.simple_triplet_matrix(stm) * lnts
    )

    ## Define attributes
    #
    class(stm) <- c('DocumentTermMatrix', 'simple_triplet_matrix')
    attr(stm, "weighting") <- c('term frequency - inverse document frequency (normalized)', 'tf-idf')

    return(stm)
}
