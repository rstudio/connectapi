% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/remote.R
\name{groups_create_remote}
\alias{groups_create_remote}
\title{Create a Remote Group}
\usage{
groups_create_remote(connect, prefix, expect = 1, check = TRUE, exact = FALSE)
}
\arguments{
\item{connect}{An R6 Connect object.}

\item{prefix}{character. The prefix of the user name to search for.}

\item{expect}{number. Optional. The number of responses to expect for this search.}

\item{check}{boolean. Optional. Whether to check for local existence first.}

\item{exact}{boolean. Optional. Whether to only create groups whose name
exactly matches the provided \code{prefix}.}
}
\value{
The results of creating the groups.
}
\description{
Create a Remote Group
}
