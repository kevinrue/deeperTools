stopifnot(suppressPackageStartupMessages({
    require(rtracklayer)
    require(GenomicRanges)
    require(SummarizedExperiment)
}))

test_that("makeExperimentFromDeeptools returns a valid object", {

    # Prepare example data ----

    # Make sample genomic ranges
    nSamples <- 3
    gr <- GRanges(
        seqnames = rep("chr1", nSamples),
        ranges = IRanges(seq(1, nSamples), 10+seq(1, nSamples)))
    names(gr) <- letters[1:3]

    # Make a sample matrix
    nFeatures <- 10
    mat <- matrix(data = rbinom(nSamples*nFeatures, 10, 0.1), nrow = nSamples)

    # Combine the two
    mcols(gr) <- mat

    # Write the sample data to file
    tf <- tempfile(fileext = ".matrix.gz")
    conn <- gzfile(tf, "wt")
    outData <- data.frame(
        seqnames(gr),
        start(gr),
        end(gr),
        names(gr),
        0,
        strand(gr),
        mcols(gr)
    )
    write.table(outData, conn, row.names = FALSE, col.names = TRUE)
    close(conn)

    # Usage ----
    out <- makeExperimentFromDeeptools(tf)

    expect_s4_class(out, "RangedSummarizedExperiment")
    expect_identical(unname(assay(out, "matrix")), mat)
    expect_identical(ranges(out), ranges(gr))
})

