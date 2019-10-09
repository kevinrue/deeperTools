#' Make a SummarizedExperiment object from a deepTools matrix
#'
#' @param file A gzip-compressed matrix file.
#' @param col.names A character vector of column names immediately set on the output object.
#'
#' @details
#' Matrix files produced by the \href{https://deeptools.readthedocs.io/en/develop/}{deepTools}
#' do not include column names that are useful to indicate the genomic position relative to a reference point, for instance.
#' While \code{colnames} could be manually set by users on the \code{SummarizedExperiment} returned,
#' it can be convenient to provide a set of column names directly to the \code{makeExperimentFromDeeptools} function,
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
#' # Make sample genomic ranges
#' nRanges <- 3
#' gr <- GRanges(
#'     seqnames=rep("chr1", nRanges),
#'     ranges=IRanges(seq(1, nRanges), 10+seq(1, nRanges)))
#' names(gr) <- letters[seq_len(nRanges)]
#'
#' # Make a sample matrix
#' nWindows <- 10
#' mat <- matrix(data=rbinom(nRanges*nWindows, 10, 0.1), nrow=nRanges)
#'
#' # Combine the two
#' mcols(gr) <- mat
#'
#' # Write the sample data to file
#' tf <- tempfile(fileext=".matrix.gz")
#' conn <- gzfile(tf, "wt")
#' outData <- data.frame(seqnames(gr), start(gr), end(gr), names(gr), 0, strand(gr), mcols(gr))
#' write.table(outData, conn, row.names=FALSE, col.names=TRUE)
#' close(conn)
#'
#' # Usage ----
#'
#' binCenters <- seq_len(nWindows) - nWindows/2
#' makeExperimentFromDeeptools(tf, col.names=binCenters)
makeExperimentFromDeeptools <- function(file, col.names=NULL) {
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
