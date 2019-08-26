#' Glue character strings
#'
#' \code{glue} collapse a character vector in a single field eliminating
#' missing values.
#'
#' @param texts (chr) character vector to collapse
#' @param collapse (chr, default = ' ') character string to separate the
#'   results
#'
#' @return a (chr) vector
#' @export
glue <- function(texts, collapse = ' ') {
    paste(stats::na.omit(c(texts)[c(texts) != ""]), collapse = collapse)
}


#' Glue unique elements
#'
#' \code{glunique} collapse a character vector in a single field,
#' eliminating missing values and duplicates elements (of the original
#' character vector)
#'
#' @param text (chr) character vector to collapse
#' @rdname glue
#' @export
glunique <- function(text) {
    glue(unique(text))
}
