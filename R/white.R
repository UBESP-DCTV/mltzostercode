#' Strip White Spaces
#'
#' \code{white} collapse all multiple "spaces" in a single space
#'
#' @param corpus (list) of documents, or a list of character vectors each one
#'  reporting tokens from a document
#'
#' @return a corpus (list)
#' @export
#'
#' @examples
white <- function(corpus){
	if(!is.list(corpus)) stop('corpus must be a list')
	#
	#
	# '\\s' := [^[:space:]] i.e. Space characters: tab, newline,
	#                       vertical tab, form feed, carriage return, space
	#                       and possibly other locale-dependent characters.
	purrr::map(corpus, function(x) stringr::str_trim(
		stringr::str_replace_all(x, '\\s+', ' ')
	))
}
