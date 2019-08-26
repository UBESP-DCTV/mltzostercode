#' csr from stm
#'
#' @param stm simple triplet matrix (with row and columns name as
#'     document and terms matrix)
#'
#' @return a matrix.csr
#' @export
csr_dtm <- function(
    stm ##
){
    ## Create a DocumentTermMatrix in a fast way (and with pryr::refs == 1)
    #
    names(stm[['dimnames']]) <- c('Docs', 'Terms')
    class(stm)               <- c(
                                    'DocumentTermMatrix',
                                    'simple_triplet_matrix'
                                 )
    attr(stm, 'weighting')   <- c('term frequency', 'tf')

    ## csr content (it is mandatory that result is integer vector!)
    #
    stm[['v']] <- as.double(stm$v)
    ia         <- cumsum(
                    c(1L, vapply(
                        X         = seq_len(stm$nrow),
                        FUN       = function(n) sum(stm$i == n),
                        FUN.VALUE = integer(1)
                    ))
                  )

    ## Create csr
    #
    methods::new('matrix.csr',
        ra        = stm$v,
        ja        = stm$j,
        ia        = ia,
        dimension = c(stm$nrow, stm$ncol)
    )
}
