as_tidy_tables_ancestry_categories <- function(tbl_json) {

  ancestry_categories <-
    tbl_json %>%
    tidyjson::gather_array() %>%
    dplyr::select(-'array.index') %>%
    tidyjson::gather_object(column.name = 'ancestry_class_symbol') %>%
    tidyjson::append_values_string(column.name = 'ancestry_class') %>%
    drop_metadata_cols() %>%
    tibble::as_tibble()

  return(ancestry_categories)
}
