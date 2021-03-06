% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/queries.R
\name{generic_search}
\alias{generic_search}
\title{A generic search for maching a variable with a value}
\usage{
generic_search(variable, value)
}
\arguments{
\item{variable}{The name of the variable}

\item{value}{The value of the varbiable}
}
\value{
A list containing the query that can be passed to \code{\link[=fetch_notices]{fetch_notices()}}.
}
\description{
Searches the \code{variable} for an exact match of the \code{value}.
}
