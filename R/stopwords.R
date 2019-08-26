#' Remove stopwords
#'
#' @param corpus (list) of documents, or a list of character vectors
#'   each one reporting tokens from a document
#' @param language (chr) optional language to pass to package \code{tm}
#'   if the parameter \code{stopwords} is left as default for
#'   determination of the stopwords (default is 'italian')
#' @param stopwords (chr) optional vectors of stopwords, by default the
#'   vector is provided by the \code{tm} package for the language
#'   choosed by \code{language} (default is italian)
#' @param stem (lgl) if \code{TRUE} perform the stemming (using the
#'   package \code{SnowballC}) of the stopwords before to remove them,
#'   default is \code{FALSE}
#' @param parallel (lgl) if \code{TRUE} perform the computation in
#'   parallel using the \code{parallel} package functionality
#'
#' @return (list) of documents pruned by the stopwords
#' @export
#'
#' @name stopwords
stopw <- function(corpus, language = 'italian', stopwords = NULL, stem = FALSE,
                  parallel = FALSE) {
#
    if (is.null(stopwords)) {
        stopwords <- tm::stopwords(language)
    }

    if (isTRUE(stem)) {
        stopwords <- SnowballC::wordStem(stopwords, language)
    }

    if (!is.list(corpus)) stop('corpus must be a list')

    ## We split the function according to the tokenized option
    #
    if (sum(sapply(corpus, function(x) sum(length(x)))) == length(corpus)) {
        ## the corpus is not tokenized so we remove the stopwords "inside"
        ## the content of the corpus.
        #
        ## In this case the stopwords will simply removed, i.e. replaced
        ## with an empty character
        #
        del_stop <- eval(parse(
            ## For each stopwords we set the replaced character as the
            ## empty one
            #
            text = paste0(
                'c(',
                paste0('"\\\\b', stopwords, '\\\\b" = ""', collapse = ", "),
                ')'
            )
        ))
        #
        if (!parallel) {
            purrr::map(corpus, ~ stringr::str_replace_all(., del_stop))
        } else {
            cl <- parallel::makePSOCKcluster(parallel::detectCores() - 1)
            #
            invisible(parallel::clusterExport(cl = cl, envir = environment(),
                varlist = c('corpus', 'del_stop')
            ))
            #
            RES <- parallel::parLapply(
                cl  = cl,
                X   = corpus,
                fun = function(doc) stringr::str_replace_all(doc, del_stop)
            )
            #
            parallel::stopCluster(cl)
            return(RES)
        } # END OF IF-ELSE (SEQ-PARALLEL not tokenized corpus)
    } else {
        ## the corpus is tokenized so we want to remove every (possible
        ## apply the procedure to the corpus so that the function will be
        ## applied to each (tokenized) document. From the document, we
        ## retain only those token for which no stopword has been detected
        ## (i.e. there is _not_ _any_ stopwords)
        #
        if (!parallel) {
            purrr::map(corpus,

                ## select only those token for which there aren't any
                ## stopwords inside
                #
                ~ .[!sapply(
                        purrr::map(
                            doc,
                            function(token) stringr::str_detect(token, del_stop)),
                        any
                )]
            )
        } else {
            cl <- parallel::makePSOCKcluster(parallel::detectCores() - 1)
            #
            invisible(parallel::clusterExport(cl = cl, envir = environment(),
                varlist = c('corpus', 'del_stop')
            ))
            #
            RES <- parallel::parLapply(
                cl  = cl,
                X   = corpus,
                fun = function(doc){
                    ## select only those token for which there aren't any
                    ## stopwords inside
                    #
                    doc[!sapply(
                            purrr::map(
                                doc,
                                function(token) stringr::str_detect(token, del_stop)),
                            any
                    )]
                }
            )
            #
            parallel::stopCluster(cl)
            return(RES)
        }
    }
}
