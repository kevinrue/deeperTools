
# averageReplicates ----

test_that("averageReplicates returns a valid object", {

    # Make a list of samples
    seList <- lapply(SAMPLE_NAMES, function(x){ importDeeptoolsExperiment(tf) })
    names(seList) <- SAMPLE_NAMES

    out <- averageReplicates(seList)

    expect_s4_class(out, "RangedSummarizedExperiment")
    avgMatrix <- 0.5 * (assay(seList[[1]]) + assay(seList[[2]]))
    expect_identical(assay(out, "matrix"), avgMatrix)
    expect_identical(ranges(out), ranges(seList[[1]]))
})
