read_file_column_names <- function(file) {

  tbl <- vroom::vroom(file,
               comment = '#',
               col_types = vroom::cols(.default = "c"),
               col_names = FALSE,
               n_max = 1L)
  unlist(tbl[1, ], use.names = FALSE)
}
