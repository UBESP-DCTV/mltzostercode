#' csr with TFiDF from stm
#'
#' @param stm simple triplet matrix (with row and colums name as
#'     document and terms matrix)
#'
#' @return a matrix.csr
#' @export
csr_dtmTfIdf <- function(
    stm
){
    ## Create a DocumentTermMatrix with TfIdf weigth (but pryr::refs!=1)
    #
    names(stm[['dimnames']]) <- c('Docs', 'Terms')
    class(stm)               <- c(
                                    'DocumentTermMatrix',
                                    'simple_triplet_matrix'
                                 )

	## Useful constants
	#
	ds <- as.integer(slam::row_sums(stm))
	ts <- as.integer(slam::col_sums(stm))

	## compute weights
	#
	## Tf
	#
	stm[['v']] <- stm[['v']]/ds[stm[['i']]]
	## Idf
	#
	lnts <- log2(stm[['nrow']]/ts)
    lnts[!is.finite(lnts)] <- 0
    ## Tf-Idf
	#
	stm <- t(t(stm) * lnts)

	## Define attributes
	#
	attr(stm, "weighting") <- c('term frequency - inverse document frequency (normalized)', 'tf-idf')

    ## csr content (it is mandatory that result is integer vector!)
    #
    ia <- cumsum(
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
