
#' Compute Window Centres
#'
#' Compute the centre of tiled windows over a range.
#'
#' @rdname windowBreaks
#'
#' @param from Start coordinate of the windows.
#' @param to End coordinate of the windows.
#' @param by Width of the windows.
#'
#' @return A numeric vector.
#' @export
#'
#' @examples
#' windowCenters(-500, 500, 10)
windowCenters <- function(from, to, by) {
    stopifnot(is.numeric(from))
    stopifnot(is.numeric(to))
    stopifnot(is.numeric(by))
    stopifnot(length(from) == 1L)
    stopifnot(length(to) == 1L)
    stopifnot(length(by) == 1L)
    seq(from + by / 2, to - by / 2, by)
}

#' Compute Window Breaks
#'
#' Compute the breaks of tiled windows over a range.
#' Identical to the base function \code{seq(from, to, by)}.
#'
#' @rdname windowBreaks
#' @export
#'
#' @examples
#' windowBreaks(-500, 500, 10)
windowBreaks <- function(from, to, by) {
    stopifnot(is.numeric(from))
    stopifnot(is.numeric(to))
    stopifnot(is.numeric(by))
    stopifnot(length(from) == 1L)
    stopifnot(length(to) == 1L)
    stopifnot(length(by) == 1L)
    seq(from, to, by)
}
