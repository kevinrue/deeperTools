
# splitByGeneSet ----

test_that("splitByGeneSet returns a valid object", {

    se <- importDeeptoolsExperiment(tf)
    genesets <- list(set1=c("GR_1"), set2=c("GR_2", "GR_3"))

    # Usage ----
    out <- splitByGeneSet(se, genesets)

    expect_type(out, "list")
    expect_length(out, length(genesets))
    expect_s4_class(out[[1]], "RangedSummarizedExperiment")
    expect_s4_class(out[[2]], "RangedSummarizedExperiment")
})
