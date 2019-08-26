#' Lowering
#'
#' \code{lowering} mutate to lowercase every documents in a corpus provided as
#' a list of texts or a list of character vectors representing tokens
#'
#' @param corpus (list) of (token of) documents
#'
#' @return a (list) of document all converted to lowercase
#' @export
lowering <- function(corpus) {

    if (!is.list(corpus)) stop('corpus must be a list')

	purrr::map(corpus, tolower)
}
