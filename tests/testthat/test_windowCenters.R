
test_that("windowCenters returns vectors of window centers", {

    out <- windowCenters(-500, 500, 10)

    expected <- seq(-495, 495, 10)
    expect_identical(out, expected)

})
