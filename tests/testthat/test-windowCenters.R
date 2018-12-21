
# windowCenters ----

test_that("windowCenters returns vectors of window centers", {

    out <- windowCenters(-500, 500, 10)

    expected <- seq(-495, 495, 10)
    expect_identical(out, expected)

})

# windowBreaks ----

test_that("windowBreaks returns vectors of window centers", {

    out <- windowBreaks(-500, 500, 10)

    expected <- seq(-500, 500, 10)
    expect_identical(out, expected)

})
