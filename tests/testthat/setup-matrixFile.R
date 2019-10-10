# Prepare example data ----

N_RANGES <- 20
N_BINS <- 10
SAMPLE_NAMES <- c("A", "B")

se_list <- generateDeeptoolsExperiments(ranges=N_RANGES, bins=N_BINS, names=SAMPLE_NAMES)

# Write example file ----

se_A <- se_list[["A"]]
gr_A <- rowRanges(se_A)
df_A <- data.frame(
  seqnames(gr_A), start(gr_A), end(gr_A), names(gr_A), 0, strand(gr_A),
  assay(se_A))

# Write the sample data to file
tf <- tempfile(fileext=".matrix.gz")
conn <- gzfile(tf, "wt")
write.table(df_A, conn, row.names=FALSE, col.names=TRUE)
close(conn)
