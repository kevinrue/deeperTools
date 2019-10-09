
#' Extracts Gene Subsets from a SummarizedExperiment object
#'
#' @param se A \code{\link{SummarizedExperiment}} object.
#' @param genesets A list of gene sets.
#'
#' @return A list of \code{\link{SummarizedExperiment}} objects.
#' @export
#'
#' @examples
#' # Prepare example data ----
#'
#' example(makeExperimentFromDeeptools)
#'
#' # Usage ----
#'
#' genesets <- list(set1=c("a"), set2=c("b", "c"))
#' sse <- splitByGeneSet(se, genesets)
splitByGeneSet <- function(se, genesets){
    outList <- list()
    for (setName in names(genesets)) {
        outList[[setName]] <- se[genesets[[setName]], ]
    }
    outList
}
