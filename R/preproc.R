#' Preprocessing data
#'
#' @param corpus (list) of documents, or a list of character vectors each one
#'  reporting tokens from a document
#'
#' @return a list of document preprocessed
#' @export
preproc <- function(corpus) {
    corpus %>%
        lowering() %>%
        tag_varicella() %>%
        nonword(numbers = TRUE) %>%
        stopw() %>%
        stem() %>%
        white() %>%
        ngram()
}
