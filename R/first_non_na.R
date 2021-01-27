#' Returns the position of the first non-NA value
#'
#' @param x An atomic vector.
#' @keywords internal
first_non_na <- function(x) {

  if(all(is.na(x))) return(NA_integer_)
  else min(which(!is.na(x)))

}
