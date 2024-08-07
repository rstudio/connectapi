% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/content.R
\name{content_render}
\alias{content_render}
\title{Render a content item.}
\usage{
content_render(content)
}
\arguments{
\item{content}{The content item you wish to render.}
}
\value{
A \link{VariantTask} object that can be used to track completion of the render.
}
\description{
Submit a request to render a content item. Once submitted, the
server runs an asynchronous process to render the content. This might be
useful if content needs to be updated after its source data has changed,
especially if this doesn't happen on a regular schedule.

Only valid for rendered content (e.g., most Quarto documents, Jupyter
notebooks, R Markdown reports).
}
