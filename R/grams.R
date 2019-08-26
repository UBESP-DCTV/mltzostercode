#' n-Gram creator
#'
#' @param corpus (list) of documents stored in a character vector of length 1
#' @param n_min (num) minimum number of words to include in the grams
#' @param n_max (num) maximum number of words to include into the grams
#' @param parallel (lgl) if \code{TRUE} perform the computation in parallel
#' using the \code{parallel} package functionality
#'
#' @name grams
#'
#' @return (list) of character vectors containing the nGrammed documents
#' @export
#'
ngram <- function (corpus, n_min = 1, n_max = 2, parallel  = FALSE) {

	if (!is.list(corpus)) stop('corpus must be a list')
	#
	if (!parallel){
		purrr::map(
			 corpus,
			 function(x) RWeka::NGramTokenizer(
                x,
                control = RWeka::Weka_control(min = n_min, max = n_max)
             )
		)
	} else {
	    if (!requireNamespace("parallel", quietly = TRUE)){
	        stop('`parallel` package required for the computation')
	    }
		cl <- parallel::makePSOCKcluster( parallel::detectCores() - 1)
		#
		invisible(parallel::clusterExport(cl = cl, envir = environment(),
			varlist = c('corpus', 'n_min', 'n_max')
		))
		#
		RES <- parallel::parLapply(
			cl  = cl,
			X   = corpus,
			fun = function(x) RWeka::NGramTokenizer(
				x ,
				control = RWeka::Weka_control(min = n_min, max = n_max)
			)
		)
		#
		parallel::stopCluster(cl)
		return(RES)
	} # END OF IF-ELSE (SEQUENTIAL - PARALLEL)
#
} # END OF FUNCTION
#


#' Bigram computation
#'
#' a shortcuts for \code{ngram} using \code{n_min = n_max = 2}
#'
#' @rdname grams
#' @return
#' @export
#'
bigram  <- function(corpus, parallel = FALSE) {
    ngram(corpus = corpus, n_min = 2, n_max = 2, parallel = parallel)
}


#' Trigram computation
#'
#' a shortcuts for \code{ngram} using \code{n_min = n_max = 3}
#'
#' @rdname grams
#' @return
#' @export
#'
trigram <- function(corpus, parallel = FALSE) {
    ngram(corpus = corpus, n_max = 3, n_max = 3, parallel = parallel)
}
