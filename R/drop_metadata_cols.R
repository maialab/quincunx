drop_metadata_cols <- function(tbl) {

  to_be_dropped <- c('..resource', '..timestamp', '..page')
  dplyr::select(tbl, -to_be_dropped)

}
