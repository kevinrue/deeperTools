
# makeExperimentFromDeeptools ----

test_that("makeExperimentFromDeeptools returns a valid object", {

    # Usage ----
    out <- makeExperimentFromDeeptools(tf, col.names=binCenters)

    expect_s4_class(out, "RangedSummarizedExperiment")
    expect_identical(unname(assay(out, "matrix")), mat)
    expect_identical(ranges(out), ranges(gr))
})

