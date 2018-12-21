
# splitByGeneSet ----

test_that("splitByGeneSet returns a valid object", {

    se <- makeExperimentFromDeeptools(tf, col.names=binCenters)
    genesets <- list(set1=c("a"), set2=c("b", "c"))

    # Usage ----
    out <- splitByGeneSet(se, genesets)

    expect_type(out, "list")
    expect_length(out, length(genesets))
    expect_s4_class(out[[1]], "RangedSummarizedExperiment")
    expect_s4_class(out[[2]], "RangedSummarizedExperiment")
})
