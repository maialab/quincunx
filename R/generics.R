#' @include class-scores.R class-publications.R class-traits.R class-performance_metrics.R class-sample_sets.R
NULL

# This is a function factory for methods that are binary endofunctions.
endofunction2 <- function(fn, obj_class) {
  function(x, y) {
    lst <- purrr::map2(s4_to_list(x), s4_to_list(y), fn)
    return(list_to_s4(lst, obj_class))
  }
}

# This is a function factory for methods that are binary predicate functions.
predicate2 <- function(fn) {
  function(x, y) {
    lst <- purrr::map2(s4_to_list(x), s4_to_list(y), fn)
    return(all(unlist(lst)))
  }
}

# This is a function factory for methods that are variadic endofunctions
# functions.
p_endofunction <- function(fn, obj_class) {
  # https://stackoverflow.com/questions/14679852/define-s4-method-with-3-dots
  function(x, ...) {
    s4_objs <- c(list(x), list(...))
    list_objs <- purrr::map(s4_objs, s4_to_list)
    list_obj <- purrr::pmap(list_objs, fn)
    return(list_to_s4(list_obj, obj_class))
  }
}

#' Set operations on PGS GWAS Catalog objects.
#'
#' Performs set union, intersection, and (asymmetric!) difference on two objects
#' of either class \linkS4class{scores}, \linkS4class{publications},
#' \linkS4class{traits}, \linkS4class{performance_metrics}, or
#' \linkS4class{sample_sets}. Note that \code{union()} removes duplicated
#' entities, whereas \code{\link[quincunx]{bind}()} does not.
#'
#' @param x,y Objects of either class \linkS4class{scores},
#'   \linkS4class{publications}, \linkS4class{traits},
#'   \linkS4class{performance_metrics}, or \linkS4class{sample_sets}.
#' @param ... other arguments passed on to methods.
#'
#' @return An object of the same class as \code{x} and \code{y}, i.e.,
#'   \linkS4class{scores}, \linkS4class{publications}, \linkS4class{traits},
#'   \linkS4class{performance_metrics}, or \linkS4class{sample_sets}.
#' @name setop
NULL

#' @rdname setop
#' @importFrom dplyr union
#' @examples
#' #
#' # union()
#' #
#' # Coming soon...
#'
#' @export
setGeneric('union', function(x, y) standardGeneric('union'))

#' @rdname setop
#' @importFrom dplyr intersect
#' @examples
#' #
#' # intersect()
#' #
#' # Coming soon...
#'
#' @export
setGeneric('intersect', function(x, y) standardGeneric('intersect'))

#' @rdname setop
#' @importFrom dplyr setdiff
#' @examples
#' #
#' # setdiff()
#' #
#' # Coming soon...
#'
#' @export
setGeneric('setdiff', function(x, y) standardGeneric('setdiff'))

#' @rdname setop
#' @importFrom dplyr setequal
#' @examples
#' #
#' # setequal()
#' #
#' # Coming soon...
#'
#' @export
setGeneric('setequal', function(x, y) standardGeneric('setequal'))


#' @export
setMethod("union",
          signature = c(x = "scores", y = "scores"),
          definition = endofunction2(dplyr::union, "scores"))

#' @export
setMethod("intersect",
          signature = c(x = "scores", y = "scores"),
          definition = endofunction2(dplyr::intersect, "scores"))

#' @export
setMethod("setdiff",
          signature = c(x = "scores", y = "scores"),
          definition = endofunction2(dplyr::setdiff, "scores"))

#' @export
setMethod("setequal",
          signature = c(x = "scores", y = "scores"),
          definition = predicate2(dplyr::setequal))

#' @export
setMethod("union",
          signature = c(x = "publications", y = "publications"),
          definition = endofunction2(dplyr::union, "publications"))

#' @export
setMethod("intersect",
          signature = c(x = "publications", y = "publications"),
          definition = endofunction2(dplyr::intersect, "publications"))

#' @export
setMethod("setdiff",
          signature = c(x = "publications", y = "publications"),
          definition = endofunction2(dplyr::setdiff, "publications"))

#' @export
setMethod("setequal",
          signature = c(x = "publications", y = "publications"),
          definition = predicate2(dplyr::setequal))

#' @export
setMethod("union",
          signature = c(x = "traits", y = "traits"),
          definition = endofunction2(dplyr::union, "traits"))

#' @export
setMethod("intersect",
          signature = c(x = "traits", y = "traits"),
          definition = endofunction2(dplyr::intersect, "traits"))

#' @export
setMethod("setdiff",
          signature = c(x = "traits", y = "traits"),
          definition = endofunction2(dplyr::setdiff, "traits"))

#' @export
setMethod("setequal",
          signature = c(x = "traits", y = "traits"),
          definition = predicate2(dplyr::setequal))

#' @export
setMethod("union",
          signature = c(x = "performance_metrics", y = "performance_metrics"),
          definition = endofunction2(dplyr::union, "performance_metrics"))

#' @export
setMethod("intersect",
          signature = c(x = "performance_metrics", y = "performance_metrics"),
          definition = endofunction2(dplyr::intersect, "performance_metrics"))

#' @export
setMethod("setdiff",
          signature = c(x = "performance_metrics", y = "performance_metrics"),
          definition = endofunction2(dplyr::setdiff, "performance_metrics"))

#' @export
setMethod("setequal",
          signature = c(x = "performance_metrics", y = "performance_metrics"),
          definition = predicate2(dplyr::setequal))

#' @export
setMethod("union",
          signature = c(x = "sample_sets", y = "sample_sets"),
          definition = endofunction2(dplyr::union, "sample_sets"))

#' @export
setMethod("intersect",
          signature = c(x = "sample_sets", y = "sample_sets"),
          definition = endofunction2(dplyr::intersect, "sample_sets"))

#' @export
setMethod("setdiff",
          signature = c(x = "sample_sets", y = "sample_sets"),
          definition = endofunction2(dplyr::setdiff, "sample_sets"))

#' @export
setMethod("setequal",
          signature = c(x = "sample_sets", y = "sample_sets"),
          definition = predicate2(dplyr::setequal))

#' @export
setMethod("union",
          signature = c(x = "cohorts", y = "cohorts"),
          definition = endofunction2(dplyr::union, "cohorts"))

#' @export
setMethod("intersect",
          signature = c(x = "cohorts", y = "cohorts"),
          definition = endofunction2(dplyr::intersect, "cohorts"))

#' @export
setMethod("setdiff",
          signature = c(x = "cohorts", y = "cohorts"),
          definition = endofunction2(dplyr::setdiff, "cohorts"))

#' @export
setMethod("setequal",
          signature = c(x = "cohorts", y = "cohorts"),
          definition = predicate2(dplyr::setequal))

#' @export
setMethod("union",
          signature = c(x = "trait_categories", y = "trait_categories"),
          definition = endofunction2(dplyr::union, "trait_categories"))

#' @export
setMethod("intersect",
          signature = c(x = "trait_categories", y = "trait_categories"),
          definition = endofunction2(dplyr::intersect, "trait_categories"))

#' @export
setMethod("setdiff",
          signature = c(x = "trait_categories", y = "trait_categories"),
          definition = endofunction2(dplyr::setdiff, "trait_categories"))

#' @export
setMethod("setequal",
          signature = c(x = "trait_categories", y = "trait_categories"),
          definition = predicate2(dplyr::setequal))

#' Bind PGS Catalog objects
#'
#' Binds together PGS Catalog objects of the same class. Note that
#' \code{bind()} preserves duplicates whereas
#' \code{\link[quincunx:setop]{union}} does not.
#'
#' @param x An object of class: \linkS4class{scores},
#'   \linkS4class{publications}, \linkS4class{traits},
#'   \linkS4class{performance_metrics}, or \linkS4class{sample_sets}.
#' @param ... Objects of the same class as \code{x}.
#'
#' @return An object of the same class as \code{x}.
#' @examples
#' # Coming soon...
#'
#' @export
setGeneric('bind', function(x, ...) standardGeneric('bind'))


#' @export
setMethod("bind",
          signature = "scores",
          definition = p_endofunction(dplyr::bind_rows, "scores"))

#' @export
setMethod("bind",
          signature = "publications",
          definition = p_endofunction(dplyr::bind_rows, "publications"))

#' @export
setMethod("bind",
          signature = "traits",
          definition = p_endofunction(dplyr::bind_rows, "traits"))

#' @export
setMethod("bind",
          signature = "performance_metrics",
          definition = p_endofunction(dplyr::bind_rows, "performance_metrics"))

#' @export
setMethod("bind",
          signature = "sample_sets",
          definition = p_endofunction(dplyr::bind_rows, "sample_sets"))

#' @export
setMethod("bind",
          signature = "cohorts",
          definition = p_endofunction(dplyr::bind_rows, "cohorts"))

#' @export
setMethod("bind",
          signature = "trait_categories",
          definition = p_endofunction(dplyr::bind_rows, "trait_categories"))

#' Filter PGS Catalog objects by identifier.
#'
#' Use \code{filter_by_id} to filter PGS Catalog objects by their respective
#' identifier (\code{id}).
#'
#' @param x An object of class either\linkS4class{scores},
#'   \linkS4class{publications}, \linkS4class{traits},
#'   \linkS4class{performance_metrics}, or \linkS4class{sample_sets}.
#' @param id Identifier.
#'
#' @return Returns an object of class either \linkS4class{scores},
#'   \linkS4class{publications}, \linkS4class{traits},
#'   \linkS4class{performance_metrics}, or \linkS4class{sample_sets}.
#' @keywords internal
setGeneric("filter_by_id", function(x, id) standardGeneric('filter_by_id'))


#' @keywords internal
setMethod("filter_by_id",
          signature(x = "scores", id = "character"),
          definition = function(x, id) {
            lst <- purrr::map(s4_to_list(x),
                              ~ dplyr::inner_join(
                                tibble::tibble(pgs_id = id),
                                .x,
                                by = 'pgs_id')
            )
            y <- list_to_s4(lst, "scores")
            return(y)
          })

#' @keywords internal
setMethod("filter_by_id",
          signature(x = "publications", id = "character"),
          definition = function(x, id) {
            lst <- purrr::map(s4_to_list(x),
                              ~ dplyr::inner_join(
                                tibble::tibble(pgp_id = id),
                                .x,
                                by = 'pgp_id')
            )
            y <- list_to_s4(lst, "publications")
            return(y)
          })

#' @keywords internal
setMethod("filter_by_id",
          signature(x = "traits", id = "character"),
          definition = function(x, id) {
            lst <- purrr::map(s4_to_list(x),
                              ~ dplyr::inner_join(
                                tibble::tibble(efo_id = id),
                                .x,
                                by = 'efo_id')
            )
            y <- list_to_s4(lst, "traits")
            return(y)
          })

#' @keywords internal
setMethod("filter_by_id",
          signature(x = "performance_metrics", id = "character"),
          definition = function(x, id) {
            lst <- purrr::map(s4_to_list(x),
                              ~ dplyr::inner_join(
                                tibble::tibble(ppm_id = id),
                                .x,
                                by = 'ppm_id')
            )
            y <- list_to_s4(lst, "performance_metrics")
            return(y)
          })

#' @keywords internal
setMethod("filter_by_id",
          signature(x = "sample_sets", id = "character"),
          definition = function(x, id) {
            lst <- purrr::map(s4_to_list(x),
                              ~ dplyr::inner_join(
                                tibble::tibble(pss_id = id),
                                .x,
                                by = 'pss_id')
            )
            y <- list_to_s4(lst, "sample_sets")
            return(y)
          })

#' @keywords internal
setMethod("filter_by_id",
          signature(x = "cohorts", id = "character"),
          definition = function(x, id) {
            lst <- purrr::map(s4_to_list(x),
                              ~ dplyr::inner_join(
                                tibble::tibble(cohort_symbol = id),
                                .x,
                                by = 'cohort_symbol')
            )
            y <- list_to_s4(lst, "cohorts")
            return(y)
          })

#' @keywords internal
setMethod("filter_by_id",
          signature(x = "trait_categories", id = "character"),
          definition = function(x, id) {
            lst <- purrr::map(s4_to_list(x),
                              ~ dplyr::inner_join(
                                tibble::tibble(trait_category = id),
                                .x,
                                by = 'trait_category')
            )
            y <- list_to_s4(lst, "trait_categories")
            return(y)
          })

#' @keywords internal
setMethod("filter_by_id",
          signature(x = "releases", id = "Date"),
          definition = function(x, id) {
            lst <- purrr::map(s4_to_list(x),
                              ~ dplyr::inner_join(
                                tibble::tibble(date = id),
                                .x,
                                by = 'date')
            )
            y <- list_to_s4(lst, "releases")
            return(y)
          })

#' @keywords internal
setMethod("filter_by_id",
          signature(x = "releases", id = "character"),
          definition = function(x, id) {
            lst <- purrr::map(s4_to_list(x),
                              ~ dplyr::inner_join(
                                tibble::tibble(date = lubridate::ymd(id)),
                                .x,
                                by = 'date')
            )
            y <- list_to_s4(lst, "releases")
            return(y)
          })

#' Subset a scores object
#'
#' You can subset \linkS4class{scores} by identifier or by position using the
#' \code{`[`} operator.
#'
#' @param x A \linkS4class{scores} object.
#' @param i Position of the identifier or the name of the identifier itself.
#' @param j Not used.
#' @param ... Additional arguments not used here.
#' @param drop Not used.
#'
#' @return A \linkS4class{scores} object.
#' @examples
#' # Coming soon...
#'
#' @name subset-scores
NULL

#' @rdname subset-scores
#' @keywords internal
#' @export
setMethod("[",
          signature(x = "scores", i = "missing", j = "missing", drop = "missing"),
          definition = function(x, i, j, ..., drop = FALSE) x)

#' @rdname subset-scores
#' @keywords internal
#' @export
setMethod("[",
          signature(x = "scores", i = "numeric", j = "missing", drop = "missing"),
          definition = function(x, i, j, ..., drop = FALSE) {
            scores_ids <- x@scores$pgs_id[i]
            filter_by_id(x, id = scores_ids)
          }
)

#' @rdname subset-scores
#' @keywords internal
#' @export
setMethod("[",
          signature(x = "scores", i = "character", j = "missing", drop = "missing"),
          definition = function(x, i, j, ..., drop = FALSE) {
            filter_by_id(x, id = i)
          }
)


#' Subset a publications object
#'
#' You can subset \linkS4class{publications} by identifier or by position using the
#' \code{`[`} operator.
#'
#' @param x A \linkS4class{publications} object.
#' @param i Position of the identifier or the name of the identifier itself.
#' @param j Not used.
#' @param ... Additional arguments not used here.
#' @param drop Not used.
#'
#' @return A \linkS4class{publications} object.
#' @examples
#' # Coming soon...
#'
#' @name subset-publications
NULL

#' @rdname subset-publications
#' @keywords internal
#' @export
setMethod("[",
          signature(x = "publications", i = "missing", j = "missing", drop = "missing"),
          definition = function(x, i, j, ..., drop = FALSE) x)

#' @rdname subset-publications
#' @keywords internal
#' @export
setMethod("[",
          signature(x = "publications", i = "numeric", j = "missing", drop = "missing"),
          definition = function(x, i, j, ..., drop = FALSE) {
            publications_ids <- x@publications$pgp_id[i]
            filter_by_id(x, id = publications_ids)
          }
)

#' @rdname subset-publications
#' @keywords internal
#' @export
setMethod("[",
          signature(x = "publications", i = "character", j = "missing", drop = "missing"),
          definition = function(x, i, j, ..., drop = FALSE) {
            filter_by_id(x, id = i)
          }
)


#' Subset a traits object
#'
#' You can subset \linkS4class{traits} by identifier or by position using the
#' \code{`[`} operator.
#'
#' @param x A \linkS4class{traits} object.
#' @param i Position of the identifier or the name of the identifier itself.
#' @param j Not used.
#' @param ... Additional arguments not used here.
#' @param drop Not used.
#'
#' @return A \linkS4class{traits} object.
#' @examples
#' # Coming soon...
#'
#' @name subset-traits
NULL

#' @rdname subset-traits
#' @keywords internal
#' @export
setMethod("[",
          signature(x = "traits", i = "missing", j = "missing", drop = "missing"),
          definition = function(x, i, j, ..., drop = FALSE) x)

#' @rdname subset-traits
#' @keywords internal
#' @export
setMethod("[",
          signature(x = "traits", i = "numeric", j = "missing", drop = "missing"),
          definition = function(x, i, j, ..., drop = FALSE) {
            traits_ids <- x@traits$efo_id[i]
            filter_by_id(x, id = traits_ids)
          }
)

#' @rdname subset-traits
#' @keywords internal
#' @export
setMethod("[",
          signature(x = "traits", i = "character", j = "missing", drop = "missing"),
          definition = function(x, i, j, ..., drop = FALSE) {
            filter_by_id(x, id = i)
          }
)


#' Subset a performance_metrics object
#'
#' You can subset \linkS4class{performance_metrics} by identifier or by position using the
#' \code{`[`} operator.
#'
#' @param x A \linkS4class{performance_metrics} object.
#' @param i Position of the identifier or the name of the identifier itself.
#' @param j Not used.
#' @param ... Additional arguments not used here.
#' @param drop Not used.
#'
#' @return A \linkS4class{performance_metrics} object.
#' @examples
#' # Coming soon...
#'
#' @name subset-performance_metrics
NULL

#' @rdname subset-performance_metrics
#' @keywords internal
#' @export
setMethod("[",
          signature(x = "performance_metrics", i = "missing", j = "missing", drop = "missing"),
          definition = function(x, i, j, ..., drop = FALSE) x)

#' @rdname subset-performance_metrics
#' @keywords internal
#' @export
setMethod("[",
          signature(x = "performance_metrics", i = "numeric", j = "missing", drop = "missing"),
          definition = function(x, i, j, ..., drop = FALSE) {
            performance_metrics_ids <- x@performance_metrics$ppm_id[i]
            filter_by_id(x, id = performance_metrics_ids)
          }
)

#' @rdname subset-performance_metrics
#' @keywords internal
#' @export
setMethod("[",
          signature(x = "performance_metrics", i = "character", j = "missing", drop = "missing"),
          definition = function(x, i, j, ..., drop = FALSE) {
            filter_by_id(x, id = i)
          }
)


#' Subset a sample_sets object
#'
#' You can subset \linkS4class{sample_sets} by identifier or by position using the
#' \code{`[`} operator.
#'
#' @param x A \linkS4class{sample_sets} object.
#' @param i Position of the identifier or the name of the identifier itself.
#' @param j Not used.
#' @param ... Additional arguments not used here.
#' @param drop Not used.
#'
#' @return A \linkS4class{sample_sets} object.
#' @examples
#' # Coming soon...
#'
#' @name subset-sample_sets
NULL

#' @rdname subset-sample_sets
#' @keywords internal
#' @export
setMethod("[",
          signature(x = "sample_sets", i = "missing", j = "missing", drop = "missing"),
          definition = function(x, i, j, ..., drop = FALSE) x)

#' @rdname subset-sample_sets
#' @keywords internal
#' @export
setMethod("[",
          signature(x = "sample_sets", i = "numeric", j = "missing", drop = "missing"),
          definition = function(x, i, j, ..., drop = FALSE) {
            sample_sets_ids <- x@sample_sets$pss_id[i]
            filter_by_id(x, id = sample_sets_ids)
          }
)

#' @rdname subset-sample_sets
#' @keywords internal
#' @export
setMethod("[",
          signature(x = "sample_sets", i = "character", j = "missing", drop = "missing"),
          definition = function(x, i, j, ..., drop = FALSE) {
            filter_by_id(x, id = i)
          }
)

#' Subset a cohorts object
#'
#' You can subset \linkS4class{cohorts} by identifier or by position using the
#' \code{`[`} operator.
#'
#' @param x A \linkS4class{cohorts} object.
#' @param i Position of the identifier or the name of the identifier itself.
#' @param j Not used.
#' @param ... Additional arguments not used here.
#' @param drop Not used.
#'
#' @return A \linkS4class{cohorts} object.
#' @examples
#' # Coming soon...
#'
#' @name subset-cohorts
NULL

#' @rdname subset-cohorts
#' @keywords internal
#' @export
setMethod("[",
          signature(x = "cohorts", i = "missing", j = "missing", drop = "missing"),
          definition = function(x, i, j, ..., drop = FALSE) x)

#' @rdname subset-cohorts
#' @keywords internal
#' @export
setMethod("[",
          signature(x = "cohorts", i = "numeric", j = "missing", drop = "missing"),
          definition = function(x, i, j, ..., drop = FALSE) {
            cohort_symbols <- x@cohorts$cohort_symbol[i]
            filter_by_id(x, id = cohort_symbols)
          }
)

#' @rdname subset-cohorts
#' @keywords internal
#' @export
setMethod("[",
          signature(x = "cohorts", i = "character", j = "missing", drop = "missing"),
          definition = function(x, i, j, ..., drop = FALSE) {
            filter_by_id(x, id = i)
          }
)

#' Subset a trait_categories object
#'
#' You can subset \linkS4class{trait_categories} by identifier or by position using the
#' \code{`[`} operator.
#'
#' @param x A \linkS4class{trait_categories} object.
#' @param i Position of the identifier or the name of the identifier itself.
#' @param j Not used.
#' @param ... Additional arguments not used here.
#' @param drop Not used.
#'
#' @return A \linkS4class{trait_categories} object.
#' @examples
#' # Coming soon...
#'
#' @name subset-trait_categories
NULL

#' @rdname subset-trait_categories
#' @keywords internal
#' @export
setMethod("[",
          signature(x = "trait_categories", i = "missing", j = "missing", drop = "missing"),
          definition = function(x, i, j, ..., drop = FALSE) x)

#' @rdname subset-trait_categories
#' @keywords internal
#' @export
setMethod("[",
          signature(x = "trait_categories", i = "numeric", j = "missing", drop = "missing"),
          definition = function(x, i, j, ..., drop = FALSE) {
            trait_categories <- x@trait_categories$trait_category[i]
            filter_by_id(x, id = trait_categories)
          }
)

#' @rdname subset-trait_categories
#' @keywords internal
#' @export
setMethod("[",
          signature(x = "trait_categories", i = "character", j = "missing", drop = "missing"),
          definition = function(x, i, j, ..., drop = FALSE) {
            filter_by_id(x, id = i)
          }
)

#' Subset a releases object
#'
#' You can subset \linkS4class{releases} by identifier or by position using the
#' \code{`[`} operator.
#'
#' @param x A \linkS4class{releases} object.
#' @param i Position of the identifier or the name of the identifier itself.
#' @param j Not used.
#' @param ... Additional arguments not used here.
#' @param drop Not used.
#'
#' @return A \linkS4class{releases} object.
#' @examples
#' # Coming soon...
#'
#' @name subset-releases
NULL

#' @rdname subset-releases
#' @keywords internal
#' @export
setMethod("[",
          signature(x = "releases", i = "missing", j = "missing", drop = "missing"),
          definition = function(x, i, j, ..., drop = FALSE) x)

#' @rdname subset-releases
#' @keywords internal
#' @export
setMethod("[",
          signature(x = "releases", i = "numeric", j = "missing", drop = "missing"),
          definition = function(x, i, j, ..., drop = FALSE) {
            release_dates <- as.character(x@releases$date[i])
            filter_by_id(x, id = release_dates)
          }
)

#' @rdname subset-releases
#' @keywords internal
#' @export
setMethod("[",
          signature(x = "releases", i = "character", j = "missing", drop = "missing"),
          definition = function(x, i, j, ..., drop = FALSE) {
            filter_by_id(x, id = i)
          }
)

#' @rdname subset-releases
#' @keywords internal
#' @export
setMethod("[",
          signature(x = "releases", i = "Date", j = "missing", drop = "missing"),
          definition = function(x, i, j, ..., drop = FALSE) {
            filter_by_id(x, id = i)
          }
)

#' Number of PGS Catalog entities
#'
#' This function returns the number of unique entities in a PGS Catalog object.
#'
#' @param x A \linkS4class{scores}, \linkS4class{publications},
#'   \linkS4class{traits}, \linkS4class{performance_metrics}, or
#'   \linkS4class{sample_sets} object.
#' @param unique Whether to count only unique entries (\code{TRUE}) or not
#'   (\code{FALSE}).
#'
#' @return An integer scalar.
#'
#' @export
setGeneric('n', function(x, unique = FALSE) standardGeneric('n'))

#' @rdname n
#' @examples
#' # Coming soon...
#'
#' @export
setMethod("n",
          signature(x = "scores"),
          definition = function(x, unique = FALSE) {
            if (unique) n <- dplyr::n_distinct(x@scores$pgs_id)
            else n <- nrow(x@scores)
            return(n)
          }
)

#' @rdname n
#' @export
setMethod("n",
          signature(x = "publications"),
          definition = function(x, unique = FALSE) {
            if (unique) n <- dplyr::n_distinct(x@publications$pgp_id)
            else n <- nrow(x@publications)
            return(n)
          }
)

#' @rdname n
#' @export
setMethod("n",
          signature(x = "traits"),
          definition = function(x, unique = FALSE) {
            if (unique) n <- dplyr::n_distinct(x@traits$efo_id)
            else n <- nrow(x@traits)
            return(n)
          }
)

#' @rdname n
#' @export
setMethod("n",
          signature(x = "performance_metrics"),
          definition = function(x, unique = FALSE) {
            if (unique) n <- dplyr::n_distinct(x@performance_metrics$ppm_id)
            else n <- nrow(x@performance_metrics)
            return(n)
          }
)

#' @rdname n
#' @export
setMethod("n",
          signature(x = "sample_sets"),
          definition = function(x, unique = FALSE) {
            if (unique) n <- dplyr::n_distinct(x@sample_sets$pss_id)
            else n <- nrow(x@sample_sets)
            return(n)
          }
)

#' @rdname n
#' @export
setMethod("n",
          signature(x = "cohorts"),
          definition = function(x, unique = FALSE) {
            if (unique) n <- dplyr::n_distinct(x@cohorts$cohort_symbol)
            else n <- nrow(x@cohorts)
            return(n)
          }
)

#' @rdname n
#' @export
setMethod("n",
          signature(x = "trait_categories"),
          definition = function(x, unique = FALSE) {
            if (unique) n <- dplyr::n_distinct(x@trait_categories$trait_category)
            else n <- nrow(x@trait_categories)
            return(n)
          }
)

#' @rdname n
#' @export
setMethod("n",
          signature(x = "releases"),
          definition = function(x, unique = FALSE) {
            if (unique) n <- dplyr::n_distinct(x@releases$date)
            else n <- nrow(x@releases)
            return(n)
          }
)
