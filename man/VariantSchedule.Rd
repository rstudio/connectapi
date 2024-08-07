% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/schedule.R
\name{VariantSchedule}
\alias{VariantSchedule}
\title{VariantSchedule}
\description{
VariantSchedule

VariantSchedule
}
\details{
An R6 class that represents a Schedule
}
\seealso{
Other R6 classes: 
\code{\link{Bundle}},
\code{\link{Content}},
\code{\link{ContentTask}},
\code{\link{Environment}},
\code{\link{PositConnect}},
\code{\link{Task}},
\code{\link{Vanity}},
\code{\link{Variant}},
\code{\link{VariantTask}}
}
\concept{R6 classes}
\section{Super classes}{
\code{connectapi::Content} -> \code{connectapi::Variant} -> \code{VariantSchedule}
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{schedule_data}}{The schedule data.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-VariantSchedule-new}{\code{VariantSchedule$new()}}
\item \href{#method-VariantSchedule-GET}{\code{VariantSchedule$GET()}}
\item \href{#method-VariantSchedule-POST}{\code{VariantSchedule$POST()}}
\item \href{#method-VariantSchedule-DELETE}{\code{VariantSchedule$DELETE()}}
\item \href{#method-VariantSchedule-set_schedule}{\code{VariantSchedule$set_schedule()}}
\item \href{#method-VariantSchedule-is_empty}{\code{VariantSchedule$is_empty()}}
\item \href{#method-VariantSchedule-print}{\code{VariantSchedule$print()}}
\item \href{#method-VariantSchedule-get_schedule}{\code{VariantSchedule$get_schedule()}}
\item \href{#method-VariantSchedule-get_schedule_remote}{\code{VariantSchedule$get_schedule_remote()}}
\item \href{#method-VariantSchedule-describe_schedule}{\code{VariantSchedule$describe_schedule()}}
\item \href{#method-VariantSchedule-clone}{\code{VariantSchedule$clone()}}
}
}
\if{html}{\out{
<details><summary>Inherited methods</summary>
<ul>
<li><span class="pkg-link" data-pkg="connectapi" data-topic="Content" data-id="bundle_delete"><a href='../../connectapi/html/Content.html#method-Content-bundle_delete'><code>connectapi::Content$bundle_delete()</code></a></span></li>
<li><span class="pkg-link" data-pkg="connectapi" data-topic="Content" data-id="bundle_download"><a href='../../connectapi/html/Content.html#method-Content-bundle_download'><code>connectapi::Content$bundle_download()</code></a></span></li>
<li><span class="pkg-link" data-pkg="connectapi" data-topic="Content" data-id="danger_delete"><a href='../../connectapi/html/Content.html#method-Content-danger_delete'><code>connectapi::Content$danger_delete()</code></a></span></li>
<li><span class="pkg-link" data-pkg="connectapi" data-topic="Content" data-id="deploy"><a href='../../connectapi/html/Content.html#method-Content-deploy'><code>connectapi::Content$deploy()</code></a></span></li>
<li><span class="pkg-link" data-pkg="connectapi" data-topic="Content" data-id="environment"><a href='../../connectapi/html/Content.html#method-Content-environment'><code>connectapi::Content$environment()</code></a></span></li>
<li><span class="pkg-link" data-pkg="connectapi" data-topic="Content" data-id="environment_all"><a href='../../connectapi/html/Content.html#method-Content-environment_all'><code>connectapi::Content$environment_all()</code></a></span></li>
<li><span class="pkg-link" data-pkg="connectapi" data-topic="Content" data-id="environment_set"><a href='../../connectapi/html/Content.html#method-Content-environment_set'><code>connectapi::Content$environment_set()</code></a></span></li>
<li><span class="pkg-link" data-pkg="connectapi" data-topic="Content" data-id="get_bundles"><a href='../../connectapi/html/Content.html#method-Content-get_bundles'><code>connectapi::Content$get_bundles()</code></a></span></li>
<li><span class="pkg-link" data-pkg="connectapi" data-topic="Content" data-id="get_connect"><a href='../../connectapi/html/Content.html#method-Content-get_connect'><code>connectapi::Content$get_connect()</code></a></span></li>
<li><span class="pkg-link" data-pkg="connectapi" data-topic="Content" data-id="get_content"><a href='../../connectapi/html/Content.html#method-Content-get_content'><code>connectapi::Content$get_content()</code></a></span></li>
<li><span class="pkg-link" data-pkg="connectapi" data-topic="Content" data-id="get_content_remote"><a href='../../connectapi/html/Content.html#method-Content-get_content_remote'><code>connectapi::Content$get_content_remote()</code></a></span></li>
<li><span class="pkg-link" data-pkg="connectapi" data-topic="Content" data-id="internal_content"><a href='../../connectapi/html/Content.html#method-Content-internal_content'><code>connectapi::Content$internal_content()</code></a></span></li>
<li><span class="pkg-link" data-pkg="connectapi" data-topic="Content" data-id="permissions"><a href='../../connectapi/html/Content.html#method-Content-permissions'><code>connectapi::Content$permissions()</code></a></span></li>
<li><span class="pkg-link" data-pkg="connectapi" data-topic="Content" data-id="permissions_add"><a href='../../connectapi/html/Content.html#method-Content-permissions_add'><code>connectapi::Content$permissions_add()</code></a></span></li>
<li><span class="pkg-link" data-pkg="connectapi" data-topic="Content" data-id="permissions_delete"><a href='../../connectapi/html/Content.html#method-Content-permissions_delete'><code>connectapi::Content$permissions_delete()</code></a></span></li>
<li><span class="pkg-link" data-pkg="connectapi" data-topic="Content" data-id="permissions_update"><a href='../../connectapi/html/Content.html#method-Content-permissions_update'><code>connectapi::Content$permissions_update()</code></a></span></li>
<li><span class="pkg-link" data-pkg="connectapi" data-topic="Content" data-id="repo_enable"><a href='../../connectapi/html/Content.html#method-Content-repo_enable'><code>connectapi::Content$repo_enable()</code></a></span></li>
<li><span class="pkg-link" data-pkg="connectapi" data-topic="Content" data-id="repo_set"><a href='../../connectapi/html/Content.html#method-Content-repo_set'><code>connectapi::Content$repo_set()</code></a></span></li>
<li><span class="pkg-link" data-pkg="connectapi" data-topic="Content" data-id="tag_delete"><a href='../../connectapi/html/Content.html#method-Content-tag_delete'><code>connectapi::Content$tag_delete()</code></a></span></li>
<li><span class="pkg-link" data-pkg="connectapi" data-topic="Content" data-id="tag_set"><a href='../../connectapi/html/Content.html#method-Content-tag_set'><code>connectapi::Content$tag_set()</code></a></span></li>
<li><span class="pkg-link" data-pkg="connectapi" data-topic="Content" data-id="tags"><a href='../../connectapi/html/Content.html#method-Content-tags'><code>connectapi::Content$tags()</code></a></span></li>
<li><span class="pkg-link" data-pkg="connectapi" data-topic="Content" data-id="update"><a href='../../connectapi/html/Content.html#method-Content-update'><code>connectapi::Content$update()</code></a></span></li>
<li><span class="pkg-link" data-pkg="connectapi" data-topic="Content" data-id="variants"><a href='../../connectapi/html/Content.html#method-Content-variants'><code>connectapi::Content$variants()</code></a></span></li>
<li><span class="pkg-link" data-pkg="connectapi" data-topic="Variant" data-id="add_subscribers"><a href='../../connectapi/html/Variant.html#method-Variant-add_subscribers'><code>connectapi::Variant$add_subscribers()</code></a></span></li>
<li><span class="pkg-link" data-pkg="connectapi" data-topic="Variant" data-id="get_dashboard_url"><a href='../../connectapi/html/Variant.html#method-Variant-get_dashboard_url'><code>connectapi::Variant$get_dashboard_url()</code></a></span></li>
<li><span class="pkg-link" data-pkg="connectapi" data-topic="Variant" data-id="get_subscribers"><a href='../../connectapi/html/Variant.html#method-Variant-get_subscribers'><code>connectapi::Variant$get_subscribers()</code></a></span></li>
<li><span class="pkg-link" data-pkg="connectapi" data-topic="Variant" data-id="get_url"><a href='../../connectapi/html/Variant.html#method-Variant-get_url'><code>connectapi::Variant$get_url()</code></a></span></li>
<li><span class="pkg-link" data-pkg="connectapi" data-topic="Variant" data-id="get_url_rev"><a href='../../connectapi/html/Variant.html#method-Variant-get_url_rev'><code>connectapi::Variant$get_url_rev()</code></a></span></li>
<li><span class="pkg-link" data-pkg="connectapi" data-topic="Variant" data-id="get_variant"><a href='../../connectapi/html/Variant.html#method-Variant-get_variant'><code>connectapi::Variant$get_variant()</code></a></span></li>
<li><span class="pkg-link" data-pkg="connectapi" data-topic="Variant" data-id="get_variant_remote"><a href='../../connectapi/html/Variant.html#method-Variant-get_variant_remote'><code>connectapi::Variant$get_variant_remote()</code></a></span></li>
<li><span class="pkg-link" data-pkg="connectapi" data-topic="Variant" data-id="job"><a href='../../connectapi/html/Variant.html#method-Variant-job'><code>connectapi::Variant$job()</code></a></span></li>
<li><span class="pkg-link" data-pkg="connectapi" data-topic="Variant" data-id="jobs"><a href='../../connectapi/html/Variant.html#method-Variant-jobs'><code>connectapi::Variant$jobs()</code></a></span></li>
<li><span class="pkg-link" data-pkg="connectapi" data-topic="Variant" data-id="remove_subscriber"><a href='../../connectapi/html/Variant.html#method-Variant-remove_subscriber'><code>connectapi::Variant$remove_subscriber()</code></a></span></li>
<li><span class="pkg-link" data-pkg="connectapi" data-topic="Variant" data-id="render"><a href='../../connectapi/html/Variant.html#method-Variant-render'><code>connectapi::Variant$render()</code></a></span></li>
<li><span class="pkg-link" data-pkg="connectapi" data-topic="Variant" data-id="renderings"><a href='../../connectapi/html/Variant.html#method-Variant-renderings'><code>connectapi::Variant$renderings()</code></a></span></li>
<li><span class="pkg-link" data-pkg="connectapi" data-topic="Variant" data-id="send_mail"><a href='../../connectapi/html/Variant.html#method-Variant-send_mail'><code>connectapi::Variant$send_mail()</code></a></span></li>
<li><span class="pkg-link" data-pkg="connectapi" data-topic="Variant" data-id="update_variant"><a href='../../connectapi/html/Variant.html#method-Variant-update_variant'><code>connectapi::Variant$update_variant()</code></a></span></li>
</ul>
</details>
}}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-VariantSchedule-new"></a>}}
\if{latex}{\out{\hypertarget{method-VariantSchedule-new}{}}}
\subsection{Method \code{new()}}{
Initialize this schedule.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{VariantSchedule$new(connect, content, key, schedule)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{connect}}{The \code{Connect} instance.}

\item{\code{content}}{The \code{Content} instance.}

\item{\code{key}}{The variant key.}

\item{\code{schedule}}{The schedule data.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-VariantSchedule-GET"></a>}}
\if{latex}{\out{\hypertarget{method-VariantSchedule-GET}{}}}
\subsection{Method \code{GET()}}{
Perform an HTTP GET request of the named API path. Returns an object parsed from the HTTP response.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{VariantSchedule$GET(path)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{path}}{API path.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-VariantSchedule-POST"></a>}}
\if{latex}{\out{\hypertarget{method-VariantSchedule-POST}{}}}
\subsection{Method \code{POST()}}{
Perform an HTTP POST request of the named API path. Returns an object parsed from the HTTP response.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{VariantSchedule$POST(path, body)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{path}}{API path.}

\item{\code{body}}{The HTTP payload.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-VariantSchedule-DELETE"></a>}}
\if{latex}{\out{\hypertarget{method-VariantSchedule-DELETE}{}}}
\subsection{Method \code{DELETE()}}{
Perform an HTTP DELETE request of the named API path. Returns the HTTP response object.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{VariantSchedule$DELETE(path)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{path}}{API path.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-VariantSchedule-set_schedule"></a>}}
\if{latex}{\out{\hypertarget{method-VariantSchedule-set_schedule}{}}}
\subsection{Method \code{set_schedule()}}{
Set the schedule for this variant
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{VariantSchedule$set_schedule(...)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{...}}{Schedule fields.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-VariantSchedule-is_empty"></a>}}
\if{latex}{\out{\hypertarget{method-VariantSchedule-is_empty}{}}}
\subsection{Method \code{is_empty()}}{
Return if this variant has a schedule.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{VariantSchedule$is_empty()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-VariantSchedule-print"></a>}}
\if{latex}{\out{\hypertarget{method-VariantSchedule-print}{}}}
\subsection{Method \code{print()}}{
Print this object.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{VariantSchedule$print(...)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{...}}{Unused.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-VariantSchedule-get_schedule"></a>}}
\if{latex}{\out{\hypertarget{method-VariantSchedule-get_schedule}{}}}
\subsection{Method \code{get_schedule()}}{
Get the schedule data.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{VariantSchedule$get_schedule()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-VariantSchedule-get_schedule_remote"></a>}}
\if{latex}{\out{\hypertarget{method-VariantSchedule-get_schedule_remote}{}}}
\subsection{Method \code{get_schedule_remote()}}{
Get and store the (remote) schedule data.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{VariantSchedule$get_schedule_remote()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-VariantSchedule-describe_schedule"></a>}}
\if{latex}{\out{\hypertarget{method-VariantSchedule-describe_schedule}{}}}
\subsection{Method \code{describe_schedule()}}{
Description of the associated schedule.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{VariantSchedule$describe_schedule()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-VariantSchedule-clone"></a>}}
\if{latex}{\out{\hypertarget{method-VariantSchedule-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{VariantSchedule$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
