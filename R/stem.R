#' Stem words
#'
#' @param corpus (list) of documents, or a list of character vectors each one
#'  reporting tokens from a document
#' @param language (chr) language to pass to package \code{Snowballc} to perform
#'  the stemming, default is 'italian'
#'
#' @return (list) of character vectors containing stemmed (tokens of the)
#'  documents
#'
#' @import magrittr
#'
#' @export
#'
stem <- function (corpus, language = 'italian') {
#
	if (!is.list(corpus)) stop('corpus must be a list')

    ## For each document, create a list to sub-tokenize each token in single
    ## words, stem them and recollapse in a single token, at the end unlisting
    ## the tokens of each documents to recreate the the original structure
    ## filled by "single-words stemmed" documents list
    #
    purrr::map(
        corpus,
        function(document) {
            sapply(
                document,
                stringi::stri_extract_all_words,
                simplify = TRUE
            )
        }
    ) %>%
    purrr::map(function(x) purrr::map(x, SnowballC::wordStem, language)) %>%
    purrr::map(function(x) purrr::map(x, paste, collapse = ' ')) %>%
    purrr::map(unlist)
}
