#' Extract the count field from a JSON response
#'
#' This function takes a string with a JSON response and looks for the pattern
#' \code{"count":(\\d+),} and returns the number. If it fails to match the
#' pattern then it returns \code{NA_integer_}.
#'
#' @param json_string a string.
#'
#' @return An integer value.
#'
#' @keywords internal
count <- function(json_string) {

  if(is.null(json_string)) return(NA_character_)

  m <- stringr::str_match(json_string, '"count":\\s*(\\d+),')
  return(as.integer(m[1,2]))
}
