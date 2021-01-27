#' Remap identifiers
#'
#' Remap identifiers
#'
#' @importFrom rlang :=
#' @keywords internal
remap_id <- function(lst_tbls, old, new) {
  id_mapping <-
    lst_tbls[[1]] %>%
    dplyr::select('..page', 'array.index', {{ old }}) %>%
    dplyr::rename({{ new }} := {{ old }})

  purrr::map(
    lst_tbls,
    ~ dplyr::left_join(., id_mapping, by = c('..page', 'array.index')) %>%
      dplyr::select(-any_of(c('document.id', 'array.index', {{old}}))) %>%
      suppressWarnings() %>%
      dplyr::relocate({{ new }}, .before = 1L)
  ) %>% return()

}
