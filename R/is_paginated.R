#' Is the response paginated?
#'
#' Checks if the response is paginated by checking if a count element exists in
#' the response.
#'
#' @param json_string a string.
#'
#' @return A logical value.
#'
#' @keywords internal
is_paginated <- function(json_string) !is.na(count(json_string))

