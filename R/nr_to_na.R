#' Convert NR (Not Recorded) to NA (Not Available)
#'
#' This function converts the \code{'NR'} string to \code{NA}.
#'
#' @param x a character vector.
#'
#' @return a character vector whose \code{'NR'} have been replaced with
#'   \code{NA}.
#'
#' @keywords internal
nr_to_na <- function(x) {

  x[x == 'NR'] <- NA_character_
  x
}
