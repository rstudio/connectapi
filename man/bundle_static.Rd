% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/deploy.R
\name{bundle_static}
\alias{bundle_static}
\title{Define a bundle from a static file (or files)}
\usage{
bundle_static(
  path,
  filename = fs::file_temp(pattern = "bundle", ext = ".tar.gz")
)
}
\arguments{
\item{path}{The path to a file (or files) that will be used for the static bundle}

\item{filename}{The output bundle path}
}
\value{
Bundle A bundle object
}
\description{
Defines a bundle from static files. It copies all files to a temporary
directory, generates a basic manifest file (using the first file as the
"primary"), and bundles the directory.
}
\details{
NOTE: the \code{rsconnect} package is required for this function to work properly.
}
\seealso{
Other deployment functions: 
\code{\link{bundle_dir}()},
\code{\link{bundle_path}()},
\code{\link{deploy}()},
\code{\link{download_bundle}()},
\code{\link{poll_task}()}
}
\concept{deployment functions}
