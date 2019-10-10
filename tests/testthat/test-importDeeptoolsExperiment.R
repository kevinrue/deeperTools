
# importDeeptoolsExperiment ----

test_that("importDeeptoolsExperiment returns a valid object", {

    # without colnames
    out <- importDeeptoolsExperiment(tf)

    expect_s4_class(out, "RangedSummarizedExperiment")
    expect_identical(colnames(out), as.character(seq_len(ncol(out))))

    # with colnames
    bin_names <- paste0("bin", seq_len(N_BINS))
    out <- importDeeptoolsExperiment(tf, bin_names)

    expect_s4_class(out, "RangedSummarizedExperiment")
    expect_identical(colnames(out), bin_names)

})

