#' Enter into a specific object and discard all other JSON data
#'
#' This function is almost the same as \code{\link[tidyjson]{enter_object}}.
#' This function has only the added benefit of automatically adding a column to
#' the data frame based on the name of the object. If the object's value is a
#' JSON array, then this function automatically iterates by calling
#' \code{\link[tidyjson]{gather_array}}. Set \code{iterable} to \code{FALSE} if
#' the object value is not an array. For more information lookup
#' \code{\link[tidyjson]{enter_object}}.
#'
#' @param tbl_json a json string or tbl_json object.
#' @param obj name of the object to be entered.
#' @param column_id_name name of the id column to be created; by default becomes
#'   the name of the object (\code{obj}) appended by \code{'_id'}.
#' @param iterable whether the object's value is an array (\code{TRUE}) or not
#'   (\code{FALSE}).
#'
#' @return a \code{\link[tidyjson]{tbl_json}} object.
#'
#' @keywords internal
enter <- function(tbl_json, obj,
                  column_id_name = paste0(obj, '_id'),
                  iterable = TRUE) {

  if(iterable) {
    tbl_json %>%
      tidyjson::enter_object({{ obj }}) %>%
      tidyjson::gather_array(column.name = column_id_name) %>%
      return()
  }
  else {
    tbl_json %>%
      tidyjson::enter_object({{ obj }}) %>%
      return()
  }
}
