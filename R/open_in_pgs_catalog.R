#' Browse PGS Catalog entities from the PGS Catalog Web Graphical User Interface
#'
#' This function launches the web browser and opens a tab for each identifier on
#' the PGS Catalog web graphical user interface: \url{https://www.pgscatalog.org/}.
#'
#' @param identifier A vector of identifiers. The identifiers can be: PGS, PGP,
#'   PSS or EFO identifiers.
#' @param pgs_catalog_entity Either \code{'pgs'} (default), \code{'pgp'},
#'   \code{'pss'}, \code{'efo'}. This argument indicates the type of the
#'   identifiers passed in \code{identifier}.
#'
#' @return Returns \code{TRUE} if successful, or \code{FALSE} otherwise. But
#'   note that this function is run for its side effect.
#' @examplesIf interactive()
#' # Open in PGS scores Catalog Web Graphical User Interface
#' open_in_pgs_catalog(c('PGS000001', 'PGS000002'))
#'
#' # Open PGS Catalog Publications
#' open_in_pgs_catalog(c('PGP000001', 'PGP000002'),
#'   pgs_catalog_entity = 'pgp')
#'
#' # Open Sample Sets (PSS)
#' open_in_pgs_catalog(c('PSS000001', 'PSS000002'),
#'   pgs_catalog_entity = 'pss')
#'
#' # Open EFO traits (EFO)
#' open_in_pgs_catalog(c('EFO_0001645', 'MONDO_0007254'),
#'   pgs_catalog_entity = 'efo')
#'
#' @export
open_in_pgs_catalog <- function(identifier = NULL,
                                pgs_catalog_entity = c(
                                  'pgs',
                                  'pgp',
                                  'pss',
                                  'efo')
) {
  url_basename <- "https://www.pgscatalog.org"

  if(is.null(identifier)) {
    utils::browseURL(glue::glue(url_basename, '/browse/all'))
    return(invisible(TRUE))
  }

  if (!(rlang::is_character(identifier)))
    stop("`identifier` must be a character vector.")

  if (any(rlang::are_na(identifier)))
    stop("The following positions of `identifier` are NAs: ",
         concatenate::cc_and(which(rlang::are_na(identifier)), oxford = TRUE),
         ".")

  pgs_catalog_entity <- rlang::arg_match(pgs_catalog_entity)

  if (interactive()) {
    msg <- 'You are about to open {length(identifier)} pages in your browser.'
    question <- glue::glue(msg)
    if (length(identifier) > 3L)
      if (!sure(before_question = question)) return(invisible(FALSE))


    entity2url <- c(
      pgs = "{url_basename}/score/{identifier}",
      pgp = "{url_basename}/publication/{identifier}",
      pss = "{url_basename}/sampleset/{identifier}",
      efo = "{url_basename}/trait/{identifier}"
    )

    urls <- glue::glue(entity2url[pgs_catalog_entity])
    purrr::walk(urls, utils::browseURL)

    return(invisible(TRUE))
  } else {
    return(invisible(TRUE))
  }
}
