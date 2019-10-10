
# averageReplicates ----

test_that("makeMetageneDataFrame returns a valid object", {

    # Make a list of samples
    sampleNames <- head(LETTERS, 2)
    binCenters <- seq_len(nWindows) - nWindows/2
    seList <- lapply(sampleNames, function(x){importDeeptoolsExperiment(tf, col.names=binCenters)})
    names(seList) <- sampleNames

    # Split each sample into gene subsets
    genesets <- list(set1=c("a"), set2=c("b", "c"))
    seList <- lapply(seList, splitByGeneSet, genesets=genesets)

    # Usage ----
    out <- makeMetageneDataFrame(seList)

    expect_s3_class(out, "data.frame")
    expect_named(out, c("sample", "geneset", "position", "metagene"))
    expect_true(all(out$sample %in% names(seList)))
    expect_true(all(out$geneset %in% names(seList[[1]])))
    expect_true(all(out$geneset %in% names(seList[[2]])))
})
