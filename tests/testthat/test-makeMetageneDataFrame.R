
# averageReplicates ----

test_that("makeMetageneDataFrame returns a valid object", {

    # Make a list of samples
    seList <- lapply(SAMPLE_NAMES, function(x){ importDeeptoolsExperiment(tf) })
    names(seList) <- SAMPLE_NAMES

    # Split each sample into gene subsets
    genesets <- list(set1=c("GR_1"), set2=c("GR_2", "GR_3"))
    seList <- lapply(seList, splitByGeneSet, genesets=genesets)

    # Usage ----
    out <- makeMetageneDataFrame(seList)

    expect_s3_class(out, "data.frame")
    expect_named(out, c("sample", "geneset", "position", "metagene"))
    expect_true(all(out$sample %in% names(seList)))
    expect_true(all(out$geneset %in% names(seList[[1]])))
    expect_true(all(out$geneset %in% names(seList[[2]])))
})
