#' Simple Triplet-Matrix to Dense matrix
#'
#' @param dtm simple triplet-matrix (document term matrix)
#' @param memory (num) the maximum amount of memory (RAM + virtual) the
#'   system is required to reserve for the process. Default is to use
#'   standard R option and so provide "memory.limit()"
#'
#' @return a full dense matrix
#' @export
#'
stm_dns <- function(dtm, memory = NULL){

    if (!is.null(memory)) utils::memory.limit(memory) #200000

    y <- matrix(as.integer(0), dtm$nrow, dtm$ncol)

    n <- 1
    while (n <= length(dtm$v)) {
        y[[dtm$i[n], dtm$j[n]]] <- dtm$v[n]
        n[1] <- n + 1
    }
    y
}
