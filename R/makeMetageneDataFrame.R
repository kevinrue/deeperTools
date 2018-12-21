#' Compute Metagenes for Lists of Samples and Gene Sets
#'
#' This function takes a list of data sets stratified by samples and gene sets.
#' It computes the metagene for each gene set in each sample, and returns
#' a \code{data.frame} suitable for \code{ggplot2}.
#'
#' @param list A named list of lists of \code{\link{SummarizedExperiment}} objects.
#' Each item in the top-most list is a named sample (e.g. "replicate1").
#' Each item within each sublist is a named gene set (e.g. "upregulated").
#' Each \code{SummarizedExperiment} stores matrix of histone modification enrichment,
#' with \code{colnames} set to the numerical value of the center of each window used to compute enrichment.
#'
#' @return A \code{data.frame} with columns \code{c("sample", "geneset", "position")}
#' @export
#' @importFrom SummarizedExperiment assay
#' @importFrom Matrix colMeans
#'
#' @seealso makeExperimentFromDeeptools
#'
#' @examples
#' # Prepare example data ----
#' example(makeExperimentFromDeeptools)
#'
#' # Make a list of samples
#' sampleNames <- head(LETTERS, 2)
#' binCenters <- seq_len(nWindows) - nWindows/2
#' seList <- lapply(sampleNames, function(x){makeExperimentFromDeeptools(tf, col.names=binCenters)})
#' names(seList) <- sampleNames
#'
#' # Split each sample into gene subsets
#' genesets <- list(set1=c("a"), set2=c("b", "c"))
#' seList <- lapply(seList, splitByGeneSet, genesets=genesets)
#'
#' # Usage ----
#' x <- makeMetageneDataFrame(seList)
#'
#' require(ggplot2)
#' ggplot(x, aes(position, metagene, color=geneset)) +
#'     geom_line(aes(group=interaction(geneset, sample))) +
#'     geom_point()
makeMetageneDataFrame <- function(list, assay.type="matrix"){
    #list: named list (by sample) of named lists (by gene set) of SummarizedExperiment objects
    globalDataFrameList <- list()
    for (sampleName in names(list)) {
        sampleDataFrameList <- list()
        sampleList <- list[[sampleName]]
        for (genesetName in names(sampleList)) {
            sampleDataFrameList[[genesetName]] <- data.frame(
                sample=sampleName,
                geneset=genesetName,
                position=as.numeric(colnames(list[[sampleName]][[genesetName]])),
                metagene=colMeans(assay(list[[sampleName]][[genesetName]], assay.type), na.rm=TRUE)
            )
        }
        globalDataFrameList[[sampleName]] <- do.call("rbind", sampleDataFrameList)
    }
    globalDataFrame <- do.call("rbind", globalDataFrameList)
    rownames(globalDataFrame) <- NULL
    globalDataFrame
}
