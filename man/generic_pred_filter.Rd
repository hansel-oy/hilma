% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/queries.R
\name{generic_pred_filter}
\alias{generic_pred_filter}
\title{A generic filter for maching a variable with a value}
\usage{
generic_pred_filter(variable, pred, value)
}
\arguments{
\item{variable}{The name of the variable}

\item{pred}{The predicate that is used for filtering}

\item{value}{The value of the varbiable}
}
\value{
A list containing the query that can be passed to \code{\link[=fetch_notices]{fetch_notices()}}
}
\description{
The \code{variable} should \code{pred} the \code{value} (e.g. \verb{name eq Hansel}).
}
