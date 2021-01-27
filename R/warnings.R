#' Pretty printing of http error
#'
#' Pretty printing of http error
#'
#' @param response A \code{\link[httr]{response}} object.
#'
#' @return This function is run for its side effect: printing.
#'
#' @keywords internal
request_warning <- function(response, warnings = FALSE) {

  code <- httr::status_code(response)
  content <- httr::content(response, 'text', encoding = 'UTF-8')
  is_json_empty <- is_json_empty(content)
  msg <- NULL

  if(!identical(code, 200L)) {

    if(identical(code, 404L)) msg <- msg_404(response)
    if(identical(code, 400L)) msg <- msg_400(response)

    if(is.null(msg)) msg <- no_msg(response)

    if (warnings) warning(msg, immediate. = TRUE, call. = FALSE)
    return(msg)
  }

  if(is_json_empty && identical(code, 200L)) {

    msg <- msg_empty(response)

    if (warnings) warning(msg, immediate. = TRUE, call. = FALSE)
    return(msg)
  }

  if(!is_json_empty && identical(code, 200L)) {

    msg <- 'OK'
    return(msg)
  }
}

#' Pretty printing of no error message
#'
#' Pretty printing of no error message
#'
#' @param response A \code{\link[httr]{response}} object.
#'
#' @return This function is run for its side effect: printing.
#'
#' @keywords internal
no_msg <- function(response) {

  url <- response$url
  code <- httr::status_code(response)
  type <- httr::http_type(response)
  content <- httr::content(response, type = "text/html", encoding = 'UTF-8')

  msg <- glue::glue(
    '\n\n',
    '* Status code:     {code}\n',
    '* Endpoint:        {url}\n',
    '* MIME type:       {type}\n',
    '* Response:        {content}\n',
    '* Message:         No message for error {code} yet!\n\n'
  )

  return(msg)
}

#' Pretty printing of empty response
#'
#' Pretty printing of empty response
#'
#' @param response A \code{\link[httr]{response}} object.
#'
#' @return This function is run for its side effect: printing.
#'
#' @keywords internal
msg_empty <- function(response) {

  url <- response$url
  code <- httr::status_code(response)
  type <- httr::http_type(response)
  content <- httr::content(response, type = "text", encoding = 'UTF-8')

  msg <- glue::glue(
    '\n\n',
    '* Status code:     {code}\n',
    '* Endpoint:        {url}\n',
    '* MIME type:       {type}\n',
    '* Response:        {content}\n',
    '* Message:         Response returned 0 results\n\n'
  )

  return(msg)
}

#' Pretty printing of 404 error
#'
#' Pretty printing of 404 error.
#'
#' @param response A \code{\link[httr]{response}} object.
#'
#' @return This function is run for its side effect: printing.
#'
#' @keywords internal
msg_404 <- function(response) {

  url <- response$url
  code <- httr::status_code(response)
  type <- httr::http_type(response)
  content <- httr::content(response, type = "text/html", encoding = 'UTF-8')
  server_error_msg <- rvest::html_text(rvest::html_nodes(content, 'h2'), trim = TRUE)

  msg <- glue::glue(
    '\n\n',
    '* Status code:    {code}\n',
    '* Endpoint:       {url}\n',
    '* MIME type:      {type}\n',
    '* Message:        {server_error_msg}\n\n'
  )

  return(msg)
}

#' Pretty printing of 404 error
#'
#' Pretty printing of 404 error.
#'
#' @param response A \code{\link[httr]{response}} object.
#'
#' @return This function is run for its side effect: printing.
#'
#' @keywords internal
msg_400 <- function(response) {

  url <- response$url
  code <- httr::status_code(response)
  type <- httr::http_type(response)
  content <- httr::content(response, type = "text/html", encoding = 'UTF-8')
  server_error_msg <- paste0(rvest::html_text(rvest::html_nodes(content, 'p'), trim = TRUE), collapse = ' ')

  msg <- glue::glue(
    '\n\n',
    '* Status code:    {code}\n',
    '* Endpoint:       {url}\n',
    '* MIME type:      {type}\n',
    '* Message:        {server_error_msg}\n\n'
  )

  return(msg)
}
