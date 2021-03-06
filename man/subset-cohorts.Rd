% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generics.R
\name{subset-cohorts}
\alias{subset-cohorts}
\alias{[,cohorts,missing,missing,missing-method}
\alias{[,cohorts,numeric,missing,missing-method}
\alias{[,cohorts,character,missing,missing-method}
\title{Subset a cohorts object}
\usage{
\S4method{[}{cohorts,missing,missing,missing}(x, i, j, ..., drop = FALSE)

\S4method{[}{cohorts,numeric,missing,missing}(x, i, j, ..., drop = FALSE)

\S4method{[}{cohorts,character,missing,missing}(x, i, j, ..., drop = FALSE)
}
\arguments{
\item{x}{A \linkS4class{cohorts} object.}

\item{i}{Position of the identifier or the name of the identifier itself.}

\item{j}{Not used.}

\item{...}{Additional arguments not used here.}

\item{drop}{Not used.}
}
\value{
A \linkS4class{cohorts} object.
}
\description{
You can subset \linkS4class{cohorts} by identifier or by position using the
\code{`[`} operator.
}
\examples{
# Coming soon...

}
\keyword{internal}
