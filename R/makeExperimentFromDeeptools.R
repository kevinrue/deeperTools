#' Make a SummarizedExperiment object from a deepTools matrix
#'
#' @param file A gzip-compressed matrix file.
#'
#' @return A \code{\linkS4class{SummarizedExperiment}} object. See \emph{Details}.
#' @export
#' @importFrom utils read.table
#' @importFrom SummarizedExperiment SummarizedExperiment
#' @importMethodsFrom SummarizedExperiment SummarizedExperiment
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
#' require(rtracklayer)
#' require(GenomicRanges)
#'
#' # Prepare example data ----
#'
#' # Make sample genomic ranges
#' nSamples <- 3
#' gr <- GRanges(
#'     seqnames = rep("chr1", nSamples),
#'     ranges = IRanges(seq(1, nSamples), 10+seq(1, nSamples)))
#' names(gr) <- letters[1:3]
#'
#' # Make a sample matrix
#' nFeatures <- 10
#' mat <- matrix(data = rbinom(nSamples*nFeatures, 10, 0.1), nrow = nSamples)
#'
#' # Combine the two
#' mcols(gr) <- mat
#'
#' # Write the sample data to file
#' tf <- tempfile(fileext = ".matrix.gz")
#' conn <- gzfile(tf, "wt")
#' outData <- data.frame(
#'     seqnames(gr),
#'     start(gr),
#'     end(gr),
#'     names(gr),
#'     0,
#'     strand(gr),
#'     mcols(gr)
#'     )
#' write.table(outData, conn, row.names = FALSE, col.names = TRUE)
#' close(conn)
#'
#' # Usage ----
#' makeExperimentFromDeeptools(tf)
makeExperimentFromDeeptools <- function(file) {
    # Parse the file
    matrixFile <- gzfile(file)
    matrixData <- read.table(matrixFile, skip = 1)
    # Extract the data matrix
    scoreMatrix <- as.matrix(matrixData[, -c(1:6)])
    # Extract the genomic range information
    rangeInfo <- with(matrixData, GRanges(seqnames = V1, ranges = IRanges(V2, V3), strand = V6))
    names(rangeInfo) <- matrixData$V4
    # Prepare the output object
    se <- SummarizedExperiment(
        assays = list(matrix=scoreMatrix),
        rowRanges=rangeInfo)
    # colnames are meaningless here
    colnames(se) <- NULL
    se
}
