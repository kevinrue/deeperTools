
# importDeeptoolsExperiment ----

test_that("importDeeptoolsExperiment returns a valid object", {

    # Usage ----
    out <- importDeeptoolsExperiment(tf, col.names=binCenters)

    expect_s4_class(out, "RangedSummarizedExperiment")
    expect_identical(unname(assay(out, "matrix")), mat)
    expect_identical(ranges(out), ranges(gr))
})

