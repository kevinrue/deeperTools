# Prepare example data ----

# Make sample genomic ranges
nRanges <- 3
gr <- GRanges(
    seqnames=rep("chr1", nRanges),
    ranges=IRanges(seq(1, nRanges), 10+seq(1, nRanges)))
names(gr) <- letters[seq_len(nRanges)]

# Make a sample matrix
nWindows <- 10
mat <- matrix(data=rbinom(nRanges*nWindows, 10, 0.1), nrow=nRanges)

# Combine the two
mcols(gr) <- mat

# Write the sample data to file
tf <- tempfile(fileext=".matrix.gz")
conn <- gzfile(tf, "wt")
outData <- data.frame(seqnames(gr), start(gr), end(gr), names(gr), 0, strand(gr), mcols(gr))
write.table(outData, conn, row.names=FALSE, col.names=TRUE)
close(conn)

binCenters <- seq_len(nWindows) - nWindows/2
