#' Number of pages
#'
#' Determine the number of pages to be requested from the total number of
#' results (\code{count}) and the number of results per page (\code{limit}). The
#' wording used here --- \code{count} and \code{limit} --- is borrowed from the
#' PGS Catalog REST API documentation.
#'
#' @param count total number of results.
#' @param limit number of results per page.
#'
#' @return The number of pages, an integer value.
#'
#' @keywords internal
n_pages <- function(count, limit = 50L) (count - 1L) %/% limit + 1L

