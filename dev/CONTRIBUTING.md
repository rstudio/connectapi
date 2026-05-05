# Contributing to connectapi

This outlines how to propose a change to connectapi. For more detailed
info about contributing to this, and other tidyverse packages, please
see the [**development contributing
guide**](https://rstd.io/tidy-contrib).

## Local development and testing

We recommend using the devtools package when developing connectapi.

``` r

install.packages("devtools")
```

`devtools::load_all()` will load the current state of the package,
simulating a user running
[`library("connectapi")`](https://posit-dev.github.io/connectapi/). Use
this when locally trying out changes to the code.

### Running tests

There are two test suites in the package. One contains unit tests and
tests that use API mocks, so you can run them without access to a
running Connect server.

Run these in R with:

``` r

devtools::test()
```

Or with the provided justfile:

    just unit-tests

A second suite runs integration tests against a live Connect server
running locally in Docker. This has some additional requirements. To run
these, you need a valid Connect license file (`.lic` file). Place it in
the root of the repository as `connect-license.lic`. You also need
Docker installed, and to install the [`with-connect`
tool](https://github.com/posit-dev/with-connect).

    uv tool install git+https://github.com/posit-dev/with-connect.git
    just integration-tests           # uses "release" by default
    just integration-tests 2024.06.0 # use a specific version

## Fixing typos

You can fix typos, spelling mistakes, or grammatical errors in the
documentation directly using the GitHub web interface, as long as the
changes are made in the *source* file. This generally means you’ll need
to edit [roxygen2
comments](https://roxygen2.r-lib.org/articles/roxygen2.html) in an `.R`,
not a `.Rd` file. You can find the `.R` file that generates the `.Rd` by
reading the comment in the first line.

## Bigger changes

If you want to make a bigger change, it’s a good idea to first file an
issue and make sure someone from the team agrees that it’s needed. If
you’ve found a bug, please file an issue that illustrates the bug with a
minimal [reprex](https://www.tidyverse.org/help/#reprex) (this will also
help you write a unit test, if needed).

### Pull request process

- Fork the package and clone onto your computer. If you haven’t done
  this before, we recommend using
  `usethis::create_from_github("posit-dev/connectapi", fork = TRUE)`.

- Install all development dependencies with
  `devtools::install_dev_deps()`, and then make sure the package passes
  R CMD check by running `devtools::check()`. If R CMD check doesn’t
  pass cleanly, it’s a good idea to ask for help before continuing.

- Create a Git branch for your pull request (PR). We recommend using
  `usethis::pr_init("brief-description-of-change")`.

- Make your changes, commit to git, and then create a PR by running
  `usethis::pr_push()`, and following the prompts in your browser. The
  title of your PR should briefly describe the change. The body of your
  PR should contain `Fixes #issue-number`.

- For user-facing changes, add a bullet to the top of `NEWS.md`
  (i.e. just below the first header). Follow the style described in
  <https://style.tidyverse.org/news.html>.

### Code style

- New code should follow the tidyverse [style
  guide](https://style.tidyverse.org). You can use the
  [styler](https://CRAN.R-project.org/package=styler) package to apply
  these styles, but please don’t restyle code that has nothing to do
  with your PR.

- We use [roxygen2](https://cran.r-project.org/package=roxygen2), with
  [Markdown
  syntax](https://cran.r-project.org/web/packages/roxygen2/vignettes/rd-formatting.html),
  for documentation.

- We use [testthat](https://cran.r-project.org/package=testthat) for
  unit tests. Contributions with test cases included are easier to
  accept.

## Code of Conduct

Please note that the connectapi project is released with a [Contributor
Code of
Conduct](https://posit-dev.github.io/connectapi/dev/CODE_OF_CONDUCT.md).
By contributing to this project you agree to abide by its terms.
