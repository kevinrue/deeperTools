
#' Compute the centre of windows
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
