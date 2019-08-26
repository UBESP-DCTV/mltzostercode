#' Create a dtm from a corpus (TF weights)
#'
#' \code{atom_dtm} take a corpus (list of documents), tokenized or not,
#' and create the Document-Term Matrix (DTM) stored as sparse, i.e.
#' using the algorithm of the simple triplet matrix (three indices i, j,
#' v, in which the indices i, j represent the row and the column
#' position of an entry and v represents its content). Moreover, for
#' compatibility reasons with some machine learning algorithm which use
#' another convention for sparsity, the indices are ordered and with
#' priority i, j too.
#'
#' @param corpus (list) of documents, or a list of character vectors
#'   each one reporting tokens from a document
#'
#' @param step (num) integer value (default is 500L) used to broken the
#'   procedure in parts of at maximum \code{step} documents each one.
#'   This is to help to don't overflow the RAM.
#'
#' @return am object of class simple_triplet_matrix (from package
#'   \code{slam}) representing a document-term matrix in which each row
#'   is a document, each columns is a term (or token) and the content is
#'   the simple frequencies of that terms in that document
#'
#' @export
atom_dtm <- function(
    corpus,
    step = 500L
){
    ## corpus must be a list
    #
    if(!is.list(corpus)) stop('corpus must be a list')

    ## Check if the corpus is a list of tokenized document or a list of
    ## full documents, in the latter case tokenize them at word level
    #
    if(sum(sapply(corpus, function(x) sum(length(x)))) == length(corpus)){
            corpus <- sapply(corpus, stringi::stri_extract_all_words)
    }

    ## The features are the single occurences of each token, ordered for a
    ## better exploratory analyses
    #
    features <- sort(unique(unlist(corpus)))

    ## we want a matrix made by documents in rows and token in columns. For
    ## Memory needed all the procedure are broken in steps of "step" length
    ## (user defined and 500 by default)
    #
    n.row <- length(corpus)
    n.col <- length(features)

    n.step <- n.row %/% step + (n.row %% step != 0)
    start  <- seq.int(1, n.row, step)
    end    <- unlist(purrr::map(start + (step - 1), min, n.row))

    ## Create an empty simple triplet (sparse) matrix
    #
    DT.triple <- slam::simple_triplet_zero_matrix(
        nrow = 0,
        ncol = n.col,
        mode = 'integer'
    )

    ## sequential creation of DTM
    #
    for (i in 1:n.step){

    message(paste0('progress: ', 100 * (i-1) / n.step, '%'))

        ## computate and save the content of the Document Term matrix for
        ## each of the steps, all to be merge together in the next step
        #
        now_length <- length(start[i]:end[i])
        #
        if(
            (!exists('DTcontent')) || (!length(DTcontent) == now_length)
        ) DTcontent <- vector('list', now_length)

        #
        ## set and activate the cluster
        #
        cl <- parallel::makePSOCKcluster(parallel::detectCores()-1)

                #
                DTcontent[1:now_length] <- parallel::parLapply(
                    cl  = cl,
                    X   = corpus[start[i]:end[i]],
                    fun = function(tokens_doc, features){
                        tabulate(
                            factor(tokens_doc,
                                levels  = features,
                                ordered = TRUE
                        ),
                        nbins = length(features)
                        )
                    },
                    features = features
                )
        #
        parallel::stopCluster(cl)

        #
        DT.triple <- slam:::rbind.simple_triplet_matrix(
            DT.triple,
            slam::as.simple_triplet_matrix(
                do.call(rbind, DTcontent)
            )
        )

        if (i == n.step) message('progress: 100%')
    } # END OF THE FOR CICLE

    ## fixes order to avoid issues with function
    ## maxent::as.compressed.matrix which required
    ## a scr standard (ordered first by row and next by colums)
    #
    ord_i <- order(DT.triple$i, DT.triple$j)
    DT.triple$i <- DT.triple$i[ord_i]
    DT.triple$j <- DT.triple$j[ord_i]
    DT.triple$v <- DT.triple$v[ord_i]
    #

#
    rownames(DT.triple) <- names(corpus)
    colnames(DT.triple) <- features                               # DTM !!!!
#
    DT.triple
#
}
