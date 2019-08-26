#' Eliminate non-words
#'
#' \code{nonword} eliminate everything is not an alphanumeric word/token
#'
#' @param corpus (list) of documents, or a list of character vectors each one
#'  reporting tokens from a document
#' @param numbers (lgl) if TRUE also numbers are removed (default FALSE)
#'
#' @return a (list) of document with non words removed
#' @export
#'
nonword <- function(corpus, numbers = FALSE) {

    if (!is.list(corpus)) stop('corpus must be a list')

    if (!numbers) {
        ## '\\W' := [^[:alnum:]_] i.e. everything not alphanumeric
        #
        purrr::map(corpus,
            function(x) stringr::str_replace_all(x, '\\W+', ' ')
        )
    } else {
        purrr::map(corpus,
            function(x) stringr::str_replace_all(x, '[^[:alpha:]_]+', ' ')
        )
    }
}
