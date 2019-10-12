#' Compute Boundaries and Center
#'
#' @description
#'
#' `binBreaks` computes the breaks of tiled windows over a range.
#' It is equal to the base function \code{seq()}.
#'
#' `binCenters` computes the centre of tiled windows over a range.
#'
#' @rdname binBreaks
#'
#' @param from Start coordinate of the windows.
#' @param to End coordinate of the windows.
#' @param width Width of the windows.
#'
#' @return For _n_ bins:
#' \describe{
#'   \item{`binBreaks`}{A numeric vector of length _n+1_ representing the breaks between bins.}
#'   \item{`binCenters`}{A numeric vector of length _n_ representing the center of each bin.}
#' }
#'
#' @export
#'
#' @examples
#' binBreaks(-500, 500, 10)
#' binCenters(-500, 500, 10)
binBreaks <- function(from, to, width) {
    stopifnot(.check_to_from_width(from, to, width))
    seq(from, to, width)
}

#' @rdname binBreaks
#' @export
binCenters <- function(from, to, width) {
    stopifnot(.check_to_from_width(from, to, width))
    seq(from + width / 2, to - width / 2, width)
}

#' Internal Validity Check of Function Arguments
#'
#' @rdname INTERNAL_check_to_from_width
#'
#' @param from Start coordinate of the windows.
#' @param to End coordinate of the windows.
#' @param width Width of the windows.
#'
#' @return `TRUE` if there are no issues. Otherwise, an error is thrown.
#' @keywords internal
.check_to_from_width <- function(from, to, width) {
    stopifnot(is.numeric(from))
    stopifnot(is.numeric(to))
    stopifnot(is.numeric(width))
    stopifnot(length(from) == 1L)
    stopifnot(length(to) == 1L)
    stopifnot(length(width) == 1L)
    stopifnot(to > from)
    if ((to-from) %% width > 0) {
        stop("The difference between 'to' and 'from' must be a multiple of 'width'")
    }
    return(TRUE)
}
