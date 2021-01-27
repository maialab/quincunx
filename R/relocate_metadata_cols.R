relocate_metadata_cols <- function(df) {

  if(tibble::is_tibble(df)) {
    dplyr::relocate(df, dplyr::starts_with('..'), .after = dplyr::last_col())
  } else { # Assuming it's a list of tibbles
    purrr::map(df, relocate_metadata_cols)
  }

}
