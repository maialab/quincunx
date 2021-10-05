#' Clear quincunx cache of memoised functions
#'
#' quincunx uses memoised functions for the REST API calls. Use this function to
#' reset the cache.
#'
#' @return Returns a logical value, indicating whether the resetting of the
#'   cache was successful (\code{TRUE}) or not  \code{FALSE}.
#'
#' @examples
#' clear_cache()
#'
#' @export
clear_cache <- function() {
  memoise::forget(memoised_GET)
}
