
#' Generate SummarizedExperiment Objects Representing deepTools Matrices
#'
#' @param ranges Number of genomic ranges (i.e., rows).
#' @param bins Number of bins (i.e., columns).
#' @param names Character vector of sample names.
#'
#' @return A list of `SummarizedExperiment` objects.
#' @export
#'
#' @importFrom stats rbinom
#' @importFrom GenomicRanges tileGenome
#'
#' @examples
#' # Usage ----
#'
#' sample_names <- c("A", "B", "C")
#'
#' se_list <- generateDeeptoolsExperiments(20, 10, sample_names)
#' se_list
generateDeeptoolsExperiments <- function(ranges, bins, names=c("A")) {
    stopifnot(is.character(names))
    if (any(duplicated(names))) {
        stop("duplicated values in 'names' are not allowed")
    }

    # Generate ranges of 1 kb each
    gr <- unlist(tileGenome(c("chrX"=ranges*1E3), ntile=ranges))
    names(gr) <- paste0("GR_", seq_len(ranges))

    res <- list()

    for (i in names) {
        # TODO: Might be fun to generate data following a realistic model
        mat <- matrix(data=rbinom(n=ranges*bins, size=10, prob=0.1), nrow=ranges)
        rownames(mat) <- names(gr)
        colnames(mat) <- seq_len(bins)

        se <- SummarizedExperiment(
            assays=list(matrix=mat),
            rowRanges=gr)

        res[[i]] <- se
    }

    return(res)
}
