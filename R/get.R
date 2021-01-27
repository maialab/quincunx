make_paginated <- function(json_txt) {

  # If paginated already then nothing to do
  if (is_paginated(json_txt)) return(json_txt)

  # Wrap in 'results' object.
  # Note: The glue() delimiters are funny to make sure they do not appear in
  #       normal JSON responses. The traditional '{' and '}' cannot be used
  #       obviously.
  return(glue::glue(
    '{"count":1,"next":null,"previous":null,"results":[!<@<!json_txt!>@>!]}',
    .open = "!<@<!", .close = "!>@>!"
  ))
}

get <- function(resource_url,
                base_url = pgs_server(),
                limit = 20L,
                verbose = FALSE,
                warnings = TRUE,
                progress_bar = TRUE) {


  if(!rlang::is_scalar_character(resource_url))
    stop('resource_url must be a single string.')

  if(!rlang::is_scalar_character(base_url))
    stop('base_url must be a single string.')

  if(!rlang::is_scalar_integer(limit) || limit < 1L || limit > 1000L)
    stop("limit must be an integer scalar between 1 and 1000!")

  # If resource is paginated get all the paginated resources.
  responses <- request_all(resource_url = resource_url,
                           base_url = base_url,
                           limit = limit,
                           verbose = verbose,
                           warnings = warnings,
                           progress_bar = progress_bar)

  resources <- purrr::map_chr(responses, ~ .$resource)
  timestamps <- purrr::map(responses, ~ .$timestamp) %>% purrr::reduce(c) # To preserve <dttm> class.
  tbl_jsons <- purrr::map_dfr(responses, ~ tidyjson::enter_object(make_paginated(.$json), 'results'))

  tbl_jsons2 <-
    tbl_jsons %>%
    dplyr::select(-'document.id') %>%
    tibble::add_column(..resource = resources,
                       ..timestamp = timestamps,
                       ..page = seq_along(resources))

  return(tbl_jsons2)
}
