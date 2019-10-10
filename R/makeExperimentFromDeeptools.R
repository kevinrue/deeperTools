#' Make a SummarizedExperiment Object from a deepTools Matrix File
#'
#' @param file A gzip-compressed matrix file.
#' @param col.names A character vector of column names immediately set on the output object.
#'
#' @details
#' Matrix files produced by the \href{https://deeptools.readthedocs.io/en/develop/}{deepTools}
#' do not include column names that are useful to indicate the genomic position relative to a reference point, for instance.
#' While \code{colnames} could be manually set by users on the \code{SummarizedExperiment} returned,
#' it can be convenient to provide a set of column names directly to the \code{importDeeptoolsExperiment} function,
#' especially when importing multiple samples in an \code{\link{lapply}} statement, for instance.
#'
#' @return A \code{\linkS4class{SummarizedExperiment}} object. See \emph{Details}.
#' @export
#' @importFrom utils read.table
#' @importFrom SummarizedExperiment SummarizedExperiment
#' @importFrom GenomicRanges GRanges
#' @importFrom IRanges IRanges
#'
#' @details The returned object contains:
#' \itemize{
#' \item The deepTools matrix in the \code{assay} slot.
#' \item The genomic range information in the \code{rowRanges} slot.
#' }
#'
#' @examples
#' library(rtracklayer)
#' library(GenomicRanges)
#'
#' # Prepare example data ----
#'
#' n_bins <- 10
#'
#' se_list <- generateDeeptoolsExperiments(ranges=20, bins=n_bins, names=c("A"))
#' se_A <- se_list[["A"]]
#'
#' # Write example file ----
#'
#' gr_A <- rowRanges(se_A)
#' df_A <- data.frame(
#'   seqnames(gr_A), start(gr_A), end(gr_A), names(gr_A), 0, strand(gr_A),
#'   assay(se_A, "matrix"))
#'
#' # Write the sample data to file
#' tf <- tempfile(fileext=".matrix.gz")
#' conn <- gzfile(tf, "wt")
#' write.table(df_A, conn, row.names=FALSE, col.names=TRUE)
#' close(conn)
#'
#' # Usage ----
#'
#' bin_centers <- seq_len(n_bins) - n_bins/2
#' importDeeptoolsExperiment(tf, col.names=bin_centers)
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
    # colnames are meaningless here
    colnames(se) <- col.names
    se
}
