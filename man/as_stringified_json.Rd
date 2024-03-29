% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{as_stringified_json}
\alias{as_stringified_json}
\title{Convert an R object to stringified JSON matching AWS Lambda conventions}
\usage{
as_stringified_json(x, ...)
}
\arguments{
\item{x}{R object to be converted to stringified JSON.}

\item{...}{additional arguments (except \code{auto_unbox} and \code{null}) passed to
\code{\link[jsonlite]{toJSON}}}
}
\value{
character
}
\description{
Stringified JSON is a string which can be parsed as a JSON. While a standard
JSON interpretation of \code{list(number = 9)} would be \code{{"number":9}},
a stringified JSON representation would be \verb{"\{\\"number\\":9\}"}.

This function will convert \code{NULL} values to JSON "nulls", to match the
convention used by Lambda event inputs, and values are automatically
unboxed.
}
\examples{
as_stringified_json(list(number = 9))
"{\"number\":9}"
}
