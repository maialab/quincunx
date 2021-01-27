#' Generate offset values
#'
#' Generate offset values to be passed to the PGS Catalog REST API endpoints.
#' The offset parameter, together with the limit parameter, allow to access to a
#' desired range of results. The offset parameter specifies the starting index
#' of the range of results desired. It is a zero based index.
#'
#' @param count total number of results.
#' @param limit number of results per page.
#'
#' @return Offset values, an integer vector.
#'
#' @keywords internal
offsets <- function(count, limit) {
  x <- seq.int(0, count - 1)
  y <- x[!duplicated(x %/% limit)]
  return(y)
}
