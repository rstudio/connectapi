% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/deploy.R
\name{Task}
\alias{Task}
\title{Task}
\description{
An R6 class that represents a Task
}
\seealso{
Other R6 classes: 
\code{\link{Bundle}},
\code{\link{Content}},
\code{\link{RStudioConnect}},
\code{\link{Vanity}},
\code{\link{VariantTask}}
}
\concept{R6 classes}
\section{Super class}{
\code{\link[connectapi:Content]{connectapi::Content}} -> \code{Task}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-new}{\code{Task$new()}}
\item \href{#method-get_task}{\code{Task$get_task()}}
\item \href{#method-print}{\code{Task$print()}}
\item \href{#method-clone}{\code{Task$clone()}}
}
}
\if{html}{
\out{<details ><summary>Inherited methods</summary>}
\itemize{
\item \out{<span class="pkg-link" data-pkg="connectapi" data-topic="Content" data-id="environment">}\href{../../connectapi/html/Content.html#method-environment}{\code{connectapi::Content$environment()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="connectapi" data-topic="Content" data-id="environment_set">}\href{../../connectapi/html/Content.html#method-environment_set}{\code{connectapi::Content$environment_set()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="connectapi" data-topic="Content" data-id="get_bundles">}\href{../../connectapi/html/Content.html#method-get_bundles}{\code{connectapi::Content$get_bundles()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="connectapi" data-topic="Content" data-id="get_connect">}\href{../../connectapi/html/Content.html#method-get_connect}{\code{connectapi::Content$get_connect()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="connectapi" data-topic="Content" data-id="get_content">}\href{../../connectapi/html/Content.html#method-get_content}{\code{connectapi::Content$get_content()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="connectapi" data-topic="Content" data-id="get_content_remote">}\href{../../connectapi/html/Content.html#method-get_content_remote}{\code{connectapi::Content$get_content_remote()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="connectapi" data-topic="Content" data-id="get_dashboard_url">}\href{../../connectapi/html/Content.html#method-get_dashboard_url}{\code{connectapi::Content$get_dashboard_url()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="connectapi" data-topic="Content" data-id="get_job">}\href{../../connectapi/html/Content.html#method-get_job}{\code{connectapi::Content$get_job()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="connectapi" data-topic="Content" data-id="get_jobs">}\href{../../connectapi/html/Content.html#method-get_jobs}{\code{connectapi::Content$get_jobs()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="connectapi" data-topic="Content" data-id="get_url">}\href{../../connectapi/html/Content.html#method-get_url}{\code{connectapi::Content$get_url()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="connectapi" data-topic="Content" data-id="job">}\href{../../connectapi/html/Content.html#method-job}{\code{connectapi::Content$job()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="connectapi" data-topic="Content" data-id="jobs">}\href{../../connectapi/html/Content.html#method-jobs}{\code{connectapi::Content$jobs()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="connectapi" data-topic="Content" data-id="runas">}\href{../../connectapi/html/Content.html#method-runas}{\code{connectapi::Content$runas()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="connectapi" data-topic="Content" data-id="tag_delete">}\href{../../connectapi/html/Content.html#method-tag_delete}{\code{connectapi::Content$tag_delete()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="connectapi" data-topic="Content" data-id="tag_set">}\href{../../connectapi/html/Content.html#method-tag_set}{\code{connectapi::Content$tag_set()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="connectapi" data-topic="Content" data-id="tags">}\href{../../connectapi/html/Content.html#method-tags}{\code{connectapi::Content$tags()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="connectapi" data-topic="Content" data-id="update">}\href{../../connectapi/html/Content.html#method-update}{\code{connectapi::Content$update()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="connectapi" data-topic="Content" data-id="variants">}\href{../../connectapi/html/Content.html#method-variants}{\code{connectapi::Content$variants()}}\out{</span>}
}
\out{</details>}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-new"></a>}}
\if{latex}{\out{\hypertarget{method-new}{}}}
\subsection{Method \code{new()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Task$new(connect, content, task)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-get_task"></a>}}
\if{latex}{\out{\hypertarget{method-get_task}{}}}
\subsection{Method \code{get_task()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Task$get_task()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-print"></a>}}
\if{latex}{\out{\hypertarget{method-print}{}}}
\subsection{Method \code{print()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Task$print(...)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-clone"></a>}}
\if{latex}{\out{\hypertarget{method-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Task$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
