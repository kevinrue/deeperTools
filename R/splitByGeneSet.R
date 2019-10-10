
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
#' sample_names <- c("A", "B")
#'
#' se_list <- generateDeeptoolsExperiments(20, 10, sample_names)
#' se_list
#'
#' # Usage ----
#'
#' range_sets <- list(set1=c("GR_1"), set2=c("GR_2", "GR_3"))
#' splitByGeneSet(se_list[[1]], range_sets)
splitByGeneSet <- function(se, genesets){
    outList <- list()
    for (setName in names(genesets)) {
        outList[[setName]] <- se[genesets[[setName]], ]
    }
    outList
}
