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

#' Set operations on PGS Catalog objects
#'
#' Performs set union, intersection, and (asymmetric!) difference on two objects
#' of either class [scores-class], [publications-class], [traits-class],
#' [performance_metrics-class], [sample_sets-class], [cohorts-class] or
#' [trait_categories-class]. Note that \code{union()} removes duplicated
#' entities, whereas [bind()] does not.
#'
#' @param x,y Objects of either class [scores-class], [publications-class],
#'   [traits-class], [performance_metrics-class], [sample_sets-class],
#'   [cohorts-class] or [trait_categories-class].
#' @param ... other arguments passed on to methods.
#'
#' @return In the case of `union()`, `intersect()`, or `setdiff()`: an object of
#'   the same class as \code{x} and \code{y}. In the case of `setequal()`, a
#'   logical scalar.
#'
#' @examplesIf interactive()
#' # Get some `scores` objects:
#' my_scores_1 <- get_scores(c('PGS000012', 'PGS000013'))
#' my_scores_2 <- get_scores(c('PGS000013', 'PGS000014'))
#'
#' #
#' # union()
#' #
#' # NB: with `union()`, PGS000013 is not repeated.
#' union(my_scores_1, my_scores_2)@scores
#'
#' #
#' # intersect()
#' #
#' intersect(my_scores_1, my_scores_2)@scores
#'
#' #
#' # setdiff()
#' #
#' setdiff(my_scores_1, my_scores_2)@scores
#'
#' #
#' # setequal()
#' #
#' setequal(my_scores_1, my_scores_2)
#' setequal(my_scores_1, my_scores_1)
#' setequal(my_scores_2, my_scores_2)
#'
#' @md
#' @name setop
NULL

#' @rdname setop
#' @importFrom dplyr union
#' @export
setGeneric('union', function(x, y) standardGeneric('union'))

#' @rdname setop
#' @importFrom dplyr intersect
#' @export
setGeneric('intersect', function(x, y) standardGeneric('intersect'))

#' @rdname setop
#' @importFrom dplyr setdiff
#' @export
setGeneric('setdiff', function(x, y) standardGeneric('setdiff'))

#' @rdname setop
#' @importFrom dplyr setequal
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
#' @param x An object of either class [scores-class], [publications-class],
#'   [traits-class], [performance_metrics-class], [sample_sets-class],
#'   [cohorts-class] or [trait_categories-class].
#' @param ... Objects of the same class as \code{x}.
#'
#' @return An object of the same class as \code{x}.
#' @md
#' @examplesIf interactive()
#' # Get some `scores` objects:
#' my_scores_1 <- get_scores(c('PGS000012', 'PGS000013'))
#' my_scores_2 <- get_scores(c('PGS000013', 'PGS000014'))
#'
#' # NB: with `bind()`, PGS000013 is repeated (as opposed to `union()`)
#' bind(my_scores_1, my_scores_2)@scores
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
#' You can subset \linkS4class{performance_metrics} by identifier or by position
#' using the \code{`[`} operator.
#'
#' @param x A \linkS4class{performance_metrics} object.
#' @param i Position of the identifier or the name of the identifier itself.
#' @param j Not used.
#' @param ... Additional arguments not used here.
#' @param drop Not used.
#'
#' @return A \linkS4class{performance_metrics} object.
#' @examplesIf interactive()
#' # Get a few performance metrics:
#' my_ppm <- get_performance_metrics(sprintf('PPM%06d', 38:42))
#'
#' #
#' # Subsetting by position
#' #
#' my_ppm[c(1, 4)]
#'
#' #
#' # Subsetting by performance metrics identifier (character)
#' #
#' my_ppm['PPM000042']
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
#' You can subset \linkS4class{sample_sets} by identifier or by position using
#' the \code{`[`} operator.
#'
#' @param x A \linkS4class{sample_sets} object.
#' @param i Position of the identifier or the name of the identifier itself.
#' @param j Not used.
#' @param ... Additional arguments not used here.
#' @param drop Not used.
#'
#' @return A \linkS4class{sample_sets} object.
#' @examplesIf interactive()
#' # Get a few sample sets:
#' my_pss <- get_sample_sets(sprintf('PSS%06d', 42:48))
#'
#' #
#' # Subsetting by position
#' #
#' my_pss[c(1, 3)]
#'
#' #
#' # Subsetting by sample set identifier (character)
#' #
#' my_pss['PSS000042']
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
#' @examplesIf interactive()
#' # Get a few cohorts by their symbol:
#' my_cohorts <- get_cohorts(c('23andMe', 'BioImage', 'Rotterdam-SI', 'SGWAS'),
#'                 progress_bar = FALSE)
#'
#' #
#' # Subsetting by position
#' #
#' my_cohorts[c(1, 3)]
#'
#' #
#' # Subsetting by cohort symbol (character)
#' #
#' my_cohorts[c('23andMe', 'SGWAS')]
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
#' You can subset \linkS4class{trait_categories} by trait category (string) or
#' by position using the \code{`[`} operator.
#'
#' @param x A \linkS4class{trait_categories} object.
#' @param i Position of the identifier or the name of the identifier itself.
#' @param j Not used.
#' @param ... Additional arguments not used here.
#' @param drop Not used.
#'
#' @return A \linkS4class{trait_categories} object.
#' @examplesIf interactive()
#' # Get details about all trait categories:
#' all_trait_categories <- get_trait_categories(progress_bar = FALSE)
#'
#' #
#' # Subsetting by position
#' #
#' all_trait_categories[1:5]
#'
#' #
#' # Subsetting by trait category (character)
#' #
#' all_trait_categories['Liver enzyme measurement']
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
#' You can subset \linkS4class{releases} by identifier (release date) or by
#' position using the \code{`[`} operator.
#'
#' @param x A \linkS4class{releases} object.
#' @param i Position of the identifier or the name of the identifier itself.
#' @param j Not used.
#' @param ... Additional arguments not used here.
#' @param drop Not used.
#'
#' @return A \linkS4class{releases} object.
#' @examplesIf interactive()
#' # Get details about all PGS Catalog data releases thus far:
#' all_releases <- get_releases(date = 'all', progress_bar = FALSE)
#'
#' #
#' # Subsetting by position
#' #
#' # Releases are, by default, sorted by date in descending order, thus the
#' # first PGS Catalog release is in the last position of the returned
#' # `all_releases` object. Here's how you can extract that first release (last
#' # position in `all_releases`):
#' all_releases[n(all_releases)]
#'
#' #
#' # Subsetting by date (character)
#' #
#' date_of_interest <- '2021-06-11'
#' class(date_of_interest)
#' all_releases[date_of_interest]
#'
#' #
#' # Subsetting by date (Date object)
#' #
#' date_of_interest <- as.Date('2021-06-11')
#' class(date_of_interest)
#' all_releases[date_of_interest]
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
#' This function returns the number of entities in a PGS Catalog object. To
#' avoid ambiguity with `dplyr::n()` use `quincunx::n()`.
#'
#' @param x A \linkS4class{scores}, \linkS4class{publications},
#'   \linkS4class{traits}, \linkS4class{performance_metrics},
#'   \linkS4class{sample_sets}, \linkS4class{cohorts},
#'   \linkS4class{trait_categories} or \linkS4class{releases} object.
#' @param unique Whether to count only unique entries (\code{TRUE}) or not
#'   (\code{FALSE}).
#'
#' @return An integer scalar.
#'
#' @docType methods
#'
#' @examplesIf interactive()
#' # Return the number of polygenic scores in a scores object:
#' my_scores <- get_scores(pgs_id = c('PGS000007', 'PGS000007', 'PGS000042'))
#' n(my_scores)
#'
#' # If you want to count unique scores only, then use the `unique` parameter:
#' n(my_scores, unique = TRUE)
#'
#' # Total number of curated publications in the PGS Catalog:
#' all_pub <- get_publications(interactive = FALSE, progress_bar = FALSE)
#' n(all_pub)
#'
#' # Total number of curated traits in the PGS Catalog:
#' all_traits <- get_traits(interactive = FALSE, progress_bar = FALSE)
#' n(all_traits)
#'
#' @md
#' @export
setGeneric('n', function(x, unique = FALSE) standardGeneric('n'))

#' @rdname n
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
