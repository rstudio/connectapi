# Development and Release Notes

The release process goes to CRAN. Details on how the release is prepared need to
be formalized and documented.

If interested in contributing, please read [CONTRIBUTING.md](CONTRIBUTING.md)

## Build Pipeline

We use GitHub Actions for the build process. Files can be seen in the
[./workflows](./workflows) directory.

We modify the examples from the
[r-lib/actions](https://github.com/r-lib/actions) repository for our purposes as
follows:

- Generally copy the examples as-is, but leave names in the `workflows` folder
consistent for history
- `R-CMD-check` is the analogue of `check-full`, without `devel` R. It
approximates CRAN submission
- `test-coverage.yaml` and `pkgdown.yaml` both have lines added to set
appropriate environment variables for "integration tests" and run Connect in
order to evaluate the package (and product) more thoroughly
- Similarly with `integration-tests.yaml`, which is analogous to `R-CMD-check`
but with integration tests too

### Integration Tests

Integration tests are designed to run the `connectapi` package against the
_actual_ latest version of Connect

- the "test environment" stands up two Connect instances as docker images using
the un-exported `build_test_env()` function
- this [uses `docker compose`](../inst/ci/) to build an environment
- we also use some ["hacky" utilities](../R/utils-ci.R) to build an initial user and API key for executing
- tests are written in the `tests/integration` directory
  - Each file should be able to be executed independently
  - There are occasional dependencies _across tests_ within a file (executed linearly)
    - (I know, this is terrible... we made choices for reasons)
  - We fudge the clean line of unit tests in order to ensure that complex
  functionalities work as expected without too much time or work
- bootstrapping the "integration test" execution happens in [the
`test-integrated.R` script](../tests/test-integrated.R), and keys mostly on the
`CONNECTAPI_INTEGRATED=true` environment variable

To run integration tests interactively / locally run the following:
```
connectapi:::build_test_env()
readRenviron(".Renviron")

# or set this in .Renviron yourself
Sys.setenv("CONNECTAPI_INTEGRATED"="true")

# or run R CMD CHECK with that env var set, etc.
source("tests/test-integrated.R")
```
