context("generateDeeptoolsExperiments")

test_that("generateDeeptoolsExperiments requires character samples names", {

    n_ranges <- 20
    n_bins <- 10
    sample_names <- seq_len(3)

    expect_error(
        generateDeeptoolsExperiments(ranges=n_ranges, bins=n_bins, names=sample_names),
        "is.character(names) is not TRUE",
        fixed=TRUE)

})

test_that("generateDeeptoolsExperiments requires unique samples names", {

    n_ranges <- 20
    n_bins <- 10
    sample_names <- rep("A", 2)

    expect_error(
        generateDeeptoolsExperiments(ranges=n_ranges, bins=n_bins, names=sample_names),
        "duplicated values in 'names' are not allowed",
        fixed=TRUE)

})

test_that("generateDeeptoolsExperiments produces the expected output", {

    n_ranges <- 20
    n_bins <- 10
    sample_names <- head(LETTERS, 3)

    out <- generateDeeptoolsExperiments(ranges=n_ranges, bins=n_bins, names=sample_names)

    expect_named(out, sample_names)
    for (i in seq_along(out)) {
        expect_true(is(out[[i]], "RangedSummarizedExperiment"))
    }

})
