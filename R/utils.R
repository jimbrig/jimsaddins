#' Null or Empty
#'
#' @family utility
#'
#' @param x value
#'
#' @return logical
#' @export
nullOrEmpty <- function(x) {
  is.null(x) || length(x) == 0
}


#' Drop Nulls or Empty's
#'
#' @param x value(s)
#'
#' @return vector
#' @export
dropNullsOrEmpty <- function(x) {
  x[!vapply(x, nullOrEmpty, FUN.VALUE = logical(1))]
}
