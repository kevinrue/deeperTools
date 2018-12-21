
# averageReplicates ----

test_that("averageReplicates returns a valid object", {

    # Make a list of samples
    sampleNames <- head(LETTERS, 2)
    binCenters <- seq_len(nWindows) - nWindows/2
    seList <- lapply(sampleNames, function(x){makeExperimentFromDeeptools(tf, col.names=binCenters)})
    names(seList) <- sampleNames

    # Usage ----
    out <- averageReplicates(seList)

    expect_s4_class(out, "RangedSummarizedExperiment")
    avgMatrix <- 0.5 * (assay(seList[[1]]) + assay(seList[[2]]))
    expect_identical(assay(out, "matrix"), avgMatrix)
    expect_identical(ranges(out), ranges(seList[[1]]))
})
