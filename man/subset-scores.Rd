% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generics.R
\name{subset-scores}
\alias{subset-scores}
\alias{[,scores,missing,missing,missing-method}
\alias{[,scores,numeric,missing,missing-method}
\alias{[,scores,character,missing,missing-method}
\title{Subset a scores object}
\usage{
\S4method{[}{scores,missing,missing,missing}(x, i, j, ..., drop = FALSE)

\S4method{[}{scores,numeric,missing,missing}(x, i, j, ..., drop = FALSE)

\S4method{[}{scores,character,missing,missing}(x, i, j, ..., drop = FALSE)
}
\arguments{
\item{x}{A \linkS4class{scores} object.}

\item{i}{Position of the identifier or the name of the identifier itself.}

\item{j}{Not used.}

\item{...}{Additional arguments not used here.}

\item{drop}{Not used.}
}
\value{
A \linkS4class{scores} object.
}
\description{
You can subset \linkS4class{scores} by identifier or by position using the
\code{`[`} operator.
}
\examples{
# Coming soon...

}
\keyword{internal}
