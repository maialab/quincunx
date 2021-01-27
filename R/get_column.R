# # This is a hack, as it is risky to be using an internal function from tidyjson:
# # tidyjson:::path()
# # https://github.com/colearendt/tidyjson/issues/134
# enter_object2 <- function (.x, ..., drop.null.json = TRUE) {
#   if (!tidyjson::is.tbl_json(.x))
#     .x <- tidyjson::as.tbl_json(.x)
#
#   path <- tidyjson:::path(...)
#   json <- tidyjson::json_get(.x)
#   json <- purrr::map(json, path %>% as.list)
#   tidyjson::tbl_json(.x, json, drop.null.json = drop.null.json)
# }
#
# get_column_chr <- function(tbl_json, json_object, col, only_col = TRUE) {
#
#   tbl_json %>%
#     enter_object2({{ json_object }}, drop.null.json = FALSE) %>%
#     tidyjson::json_get_column(column_name = {{ col }}) %>%
#     dplyr::mutate({{ col }} := purrr::map(.data[[col]], as.character)) %>%
#     `if`(only_col, dplyr::select(., col), .) %>%
#     tidyjson::as_tibble() # as_tibble necessary to drop ..JSON col.
# }
#
# get_column_int <- function(tbl_json, json_object, col, only_col = TRUE) {
#
#   tbl_json %>%
#     enter_object2({{ json_object }}, drop.null.json = FALSE) %>%
#     tidyjson::json_get_column(column_name = {{ col }}) %>%
#     dplyr::mutate({{ col }} := purrr::map(.data[[col]], as.integer)) %>%
#     `if`(only_col, dplyr::select(., col), .) %>%
#     tidyjson::as_tibble() # as_tibble necessary to drop ..JSON col.
# }
#
# get_column_dbl <- function(tbl_json, json_object, col, only_col = TRUE) {
#
#   tbl_json %>%
#     enter_object2({{ json_object }}, drop.null.json = FALSE) %>%
#     tidyjson::json_get_column(column_name = {{ col }}) %>%
#     dplyr::mutate({{ col }} := purrr::map(.data[[col]], as.double)) %>%
#     `if`(only_col, dplyr::select(., col), .) %>%
#     tidyjson::as_tibble() # as_tibble necessary to drop ..JSON col.
# }
#
# get_column_lgl <- function(tbl_json, json_object, col, only_col = TRUE) {
#
#   tbl_json %>%
#     enter_object2({{ json_object }}, drop.null.json = FALSE) %>%
#     tidyjson::json_get_column(column_name = {{ col }}) %>%
#     dplyr::mutate({{ col }} := purrr::map(.data[[col]], as.logical)) %>%
#     `if`(only_col, dplyr::select(., col), .) %>%
#     tidyjson::as_tibble() # as_tibble necessary to drop ..JSON col.
# }
