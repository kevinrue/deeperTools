
# binCenters ----

test_that("binCenters returns vectors of window centers", {

    out <- binCenters(-500, 500, 10)

    expected <- seq(-495, 495, 10)
    expect_identical(out, expected)

})

# binBreaks ----

test_that("binBreaks returns vectors of window centers", {

    out <- binBreaks(-500, 500, 10)

    expected <- seq(-500, 500, 10)
    expect_identical(out, expected)

})

# .check_to_from_width ----

test_that(".check_to_from_width throws an error if breaks do not match exactly", {

    expect_error(
        binCenters(-10, 501, 10),
        "The difference between 'to' and 'from' must be a multiple of 'width'",
        fixed=TRUE
    )

})
