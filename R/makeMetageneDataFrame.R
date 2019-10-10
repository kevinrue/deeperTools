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
#' @param assay.type Name the assay to average between all samples.
#'
#' @return A \code{data.frame} with columns \code{c("sample", "geneset", "position")}
#' @export
#' @importFrom SummarizedExperiment assay
#' @importFrom Matrix colMeans
#'
#' @seealso importDeeptoolsExperiment
#'
#' @examples
#' # Set the plotting environment ----
#'
#' library(ggplot2)
#' library(cowplot)
#' theme_set(theme_cowplot())
#'
#' # Prepare example data ----
#'
#' sample_names <- c("A", "B", "C")
#'
#' se_list <- generateDeeptoolsExperiments(20, 10, sample_names)
#' se_list
#'
#' # Split each sample into gene subsets ----
#'
#' range_sets <- list(set1=c("GR_1"), set2=c("GR_2", "GR_3"))
#' se_list_list <- lapply(se_list, splitByGeneSet, range_sets)
#'
#' # Usage ----
#'
#' x <- makeMetageneDataFrame(se_list_list)
#'
#' ggplot(x, aes(position, metagene, color=geneset)) +
#'     geom_line(aes(group=interaction(geneset, sample))) +
#'     geom_point() +
#'     facet_wrap(~sample, ncol=1)
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
