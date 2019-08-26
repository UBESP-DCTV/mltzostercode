#' Tag varicella
#'
#' @param corpus (list) of documents, or a list of character vectors
#'     each one reporting tokens from a document
#'
#' @return a list of document with uniform tag for "varicella"
#' @export
tag_varicella <- function(corpus) {
    corpus %>%
        purrr::map(~gsub(
            pattern     = '\\w*varicel\\w*',
            replacement = ' varicella ',
            x           = .
        )) %>%
        purrr::map(~gsub(
            pattern     = '<?052>?',
            replacement = ' varicella ',
            x           = .
        )) %>%
        purrr::map(~gsub(
            pattern     = '( *varicella *)+',
            replacement = ' varicella ',
            x           = .
        )) %>%
        purrr::map(~gsub(
            pattern     = '\\b[[:alnum:]]{1,2}\\b',
            replacement = ' ',
            x           = .
        ))
}
