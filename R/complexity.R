#' Computational complexity for code
#'
#' \code{complexity} compute the required time and memory used to run code
#'
#' @param code a valid R expression
#'
#' @return
#' @export
#'
#' @examples
#'
complexity <- function (code) {
    ppt <- function(y) {
        if (!is.na(y[4L]))
            y[1L] <- y[1L] + y[4L]
        if (!is.na(y[5L]))
            y[2L] <- y[2L] + y[5L]
        y[1L:3L]
    }

    if (!exists("proc.time"))
        return(rep(NA_real_, 5L))

    time <- ppt(proc.time())
    expr <- substitute(code)
    rm(code)
    gc(FALSE, TRUE)
    memory   <- pryr::mem_used()
    memory[] <- pryr::mem_used()

    time[] <- ppt(proc.time())

    on.exit(cat('Timing stopped at:'))
    on.exit(cat(ppt(proc.time() - time),
        '\n'),
    add = TRUE
    )

    on.exit(cat('Memory stopped at:'), add = TRUE)
    on.exit(pryr:::print.bytes(pryr::mem_used() - memory),
        add = TRUE
    )

    eval(expr, parent.frame())

    time[] <- ppt(proc.time()) - time

    on.exit()

    memory[] <- pryr::mem_used() - memory[]

    list(
        time_used   = time,
        memory_used = memory
    )

}

