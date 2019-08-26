#' Lowering
#'
#' \code{lowering} mutate to lowercase every documents in a corpus provided as
#' a list of texts or a list of character vectors rappresenting tokens
#'
#' @param corpus (list) of (token of) documents
#'
#' @return
#' @export
#'
#' @examples
lowering <- function (corpus) {

    if(!is.list(corpus)) stop('corpus must be a list')

	purrr::map(corpus, tolower)
}
