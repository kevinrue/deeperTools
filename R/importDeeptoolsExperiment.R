#' Import a deepTools Matrix File as a SummarizedExperiment Object
#'
#' @param file A gzip-compressed matrix file.
#' @param col.names A character vector of column names immediately set on the output object.
#'
#' @details
#' Matrix files produced by [deepTools](https://deeptools.readthedocs.io/en/develop/) do not include column names that are useful to indicate the genomic position relative to a reference point, for instance.
#' While `colnames` could be manually set by users on the [SummarizedExperiment][SummarizedExperiment::SummarizedExperiment-class] returned, it can be convenient to provide a set of column names directly to the [importDeeptoolsExperiment()] function, especially when importing multiple samples in an [lapply()] statement, for instance.
#'
#' @return A [SummarizedExperiment][SummarizedExperiment::SummarizedExperiment-class] object.
#' See _Details_.
#'
#' @export
#' @importFrom utils read.table
#' @importFrom SummarizedExperiment SummarizedExperiment
#' @importFrom GenomicRanges GRanges
#' @importFrom IRanges IRanges
#'
#' @details The returned object contains:
#' * The deepTools matrix in the `assay` slot.
#' * The genomic range information in the `rowRanges` slot.
#'
#' @examples
#' library(rtracklayer)
#' library(GenomicRanges)
#'
#' # Prepare example data ----
#'
#' se_list <- generateDeeptoolsExperiments(ranges=20, bins=10, names=c("A"))
#' se_list
#'
#' # Write example file ----
#'
#' se_A <- se_list[["A"]]
#' gr_A <- rowRanges(se_A)
#' df_A <- data.frame(
#'   seqnames(gr_A), start(gr_A), end(gr_A), names(gr_A), 0, strand(gr_A),
#'   assay(se_A))
#'
#' # Write the sample data to file
#' tf <- tempfile(fileext=".matrix.gz")
#' conn <- gzfile(tf, "wt")
#' write.table(df_A, conn, row.names=FALSE, col.names=TRUE)
#' close(conn)
#'
#' # Usage ----
#'
#' importDeeptoolsExperiment(tf)
importDeeptoolsExperiment <- function(file, col.names=NULL) {
    # Parse the file
    matrixFile <- gzfile(file)
    matrixData <- read.table(matrixFile, skip=1)
    # Extract the data matrix
    scoreMatrix <- as.matrix(matrixData[, -seq_len(6)])
    # Extract the genomic range information
    rangeInfo <- with(matrixData, GRanges(seqnames=V1, ranges=IRanges(V2, V3), strand=V6))
    names(rangeInfo) <- matrixData$V4
    # Prepare the output object
    se <- SummarizedExperiment(
        assays=list(matrix=scoreMatrix),
        rowRanges=rangeInfo)
    # colnames represent bin identifiers/position
    if (is.null(col.names)) {
        col.names <- seq_len(ncol(se))
    }
    colnames(se) <- col.names
    se
}
