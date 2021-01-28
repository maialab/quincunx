#' PGS REST API server
#'
#' @return A string containing PGS REST API server URL.
#' @keywords internal
pgs_server <- function() 'https://www.pgscatalog.org'

#' User agent identification
#'
#' Generates an S3 \code{request} object as defined by the package \code{httr},
#' that is used to identify this package as the user agent in requests to the
#' PGS REST API. The user agent identification string is: \code{"quincunx: R
#' Client for the PGS REST API"}.
#'
#' @return An S3 \code{request} object as defined by the package \code{httr}.
#' @keywords internal
user_agent_id <- function()
  httr::user_agent("quincunx: R client for the PGS Catalog REST API")

#' Request a resource from the PGS REST API
#'
#' Performs a \code{\link[httr]{GET}} request on the endpoint as specified by
#' \code{resource_url}.
#'
#' @param resource_url Endpoint URL. The endpoint is internally appended to the
#'   \code{base_url}. It should start with a forward slash (\code{'/'}).
#' @param base_url The PGS REST API base URL.
#' @param user_agent User agent.
#' @param verbose Whether to be verbose.
#' @param warnings Whether to print warnings.
#'
#' @return A named list of four elements:
#' \describe{
#' \item{resource}{The URL endpoint.}
#' \item{code}{\href{https://tinyurl.com/8yqvhwf}{HTTP
#' status code}.}
#' \item{message}{A string describing the status of the response obtained:
#' \code{'OK'} if successful or a description of the error.}
#' \item{json}{JSON response as string.}
#' }
#'
#' @keywords internal
request <- function(resource_url,
                    base_url = pgs_server(),
                    user_agent = user_agent_id(),
                    verbose = FALSE,
                    warnings = TRUE) {

  url <- stringr::str_c(base_url, resource_url)

  # Add the format flag to ensure json responses only
  url <- ifelse(
    contains_question_mark(url),
    glue::glue('{url}&format=json'),
    glue::glue('{url}?format=json')
  )

  response <- httr::GET(url, user_agent)
  timestamp <- Sys.time()

  if (verbose) {
    msg <- request_msg(resource_url, base_url, user_agent, response)
    message(msg)
  }

  obj <- list(resource = response$url,
              code = httr::status_code(response),
              message = NA_character_,
              json = NULL,
              timestamp = timestamp
              )

  # request_warning() will trigger a warning if `warnings` is TRUE
  # and if response is not code 200, or if it is empty.
  msg <- request_warning(response, warnings = warnings)
  obj$message <- msg

  if(identical(obj$code, 200L))
    obj$json <- httr::content(response, 'text', encoding = 'UTF-8')

    return(obj)
}

#' Request a paginated resource from the PGS REST API
#'
#' Performs a GET request on the specified \code{resource_url} and all its
#' pages.
#'
#' @param resource_url Endpoint URL. The endpoint is internally appended to the
#'   \code{base_url}. It should start with a forward slash (\code{/}).
#' @param base_url The PGS REST API base URL (one should not need to change its
#'   default value).
#' @param limit number of results per page.
#' @param verbose whether to print information about each API request.
#' @param warnings whether to print warnings related to API requests.
#' @param progress_bar whether to show a progress bar as the paginated resources
#'   are retrieved.
#'
#' @return A list four named elements:
#' \describe{
#' \item{resource}{The URL endpoint.}
#' \item{code}{\href{https://tinyurl.com/8yqvhwf}{HTTP
#' status code}.}
#' \item{message}{A string describing the status of the response obtained. It is
#' "OK" if everything went OK or some other string describing the problem.}
#' \item{json}{A list of JSON responses (each response is a string).}
#' }
#'
#' @keywords internal
request_all <- function(resource_url = "/",
                           base_url = pgs_server(),
                           limit = 20L,
                           verbose = FALSE,
                           warnings = TRUE,
                           progress_bar = TRUE) {

  # `response` object for initial assessing of pages and count
  obj <- request(resource_url = resource_url,
                 base_url = base_url,
                 verbose = verbose,
                 warnings = warnings)

  # Firstly, we check if response is empty.
  if(is_json_empty(obj$json)) {
    # Because the response was empty, there are not pages to follow
    # so we return the response as is, wrapped in list(), indicating
    # only the first response was obtainable.
    return(list(obj))
  }

  # Secondly, we check if it's paginated
  # Is the response paginated?
  is_it_paginated <- is_paginated(obj$json)

  # If it is not paginated return the obtained object.
  if(!is_it_paginated) return(list(obj))

  # Else, let's go after for those paginated resources.
  count <- count(obj$json)
  n_pages <- n_pages(count = count, limit = limit)

  # Check if limit is within 1 and 1000.
  if(!rlang::is_scalar_integer(limit) || limit < 1L || limit > 1000L)
    stop("limit must be an integer scalar between 1 and 1000!")

  # Now we calculate the number the offsets
  offsets <- offsets(count = count, limit = limit)

  # Generate each individual resource URL one per page
  if(contains_question_mark(resource_url)) {
    resource_url_by_page <- sprintf("%s&offset=%d&limit=%d", resource_url, offsets, limit)
  } else {
    resource_url_by_page <- sprintf("%s?offset=%d&limit=%d", resource_url, offsets, limit)
  }

  # Progress bar
  if(progress_bar) {
    pb <- progress::progress_bar$new(total = n_pages,
                                     show_after = 0,
                                     format = "  downloading [:bar] :percent eta: :eta")
    pb$tick(0)
    request2 <- function(resource_url = "/",
                            base_url = base_url,
                            verbose = FALSE,
                            warnings = TRUE) {

      res <- request(resource_url = resource_url,
                 base_url = base_url,
                 verbose = verbose,
                 warnings = warnings)
      pb$tick()
      return(res)
    }
  } else {
    request2 <- request
  }


  # Each element of this list is a returned object from a page
  objs <-purrr::map(resource_url_by_page,
                    request2,
                    base_url = base_url,
                    verbose = verbose,
                    warnings = warnings)

  is_ok <- purrr::map_lgl(objs, ~ identical(.x$message, 'OK'))

  if(!all(is_ok))
    stop(glue::glue("Failed to get all pages of {resource_url}!"))

  return(objs)
}
