#' Average Assays Across Replicates
#'
#' This function takes a list of [SummarizedExperiment][SummarizedExperiment::SummarizedExperiment-class] objects.
#' It averages the requested assay between all input objects and returns a new single [SummarizedExperiment][SummarizedExperiment::SummarizedExperiment-class] object with the averaged assay.
#'
#' @param list A list of [SummarizedExperiment][SummarizedExperiment::SummarizedExperiment-class] objects.
#' @param assay.type Name the assay to average between all samples.
#'
#' @return A [SummarizedExperiment][SummarizedExperiment::SummarizedExperiment-class] object with the averaged assay.
#' @export
#'
#' @examples
#' # Prepare example data ----
#'
#' sample_names <- c("A", "B", "C")
#'
#' se_list <- generateDeeptoolsExperiments(20, 10, sample_names)
#' se_list
#'
#' # Usage ----
#'
#' averageReplicates(se_list)
averageReplicates <- function(list, assay.type="matrix"){
    outAssayList <- list()

    numberRows <- nrow(list[[1]])
    numberColumns <- ncol(list[[1]])
    outMatrix <- matrix(0, numberRows, numberColumns)
    for (i in seq_along(list)) {
        outMatrix <- outMatrix + assay(list[[i]], assay.type)
    }
    outAssayList[[assay.type]] <- outMatrix / length(list)

    se <- SummarizedExperiment(assays=outAssayList, rowRanges=rowRanges(list[[1]]))
    # Transfer the dimnames
    rownames(se) <- rownames(list[[1]])
    colnames(se) <- colnames(list[[1]])
    se
}
