# Development and Release Notes

The release process goes to CRAN. Details on how the release is prepared need to
be formalized and documented.

If interested in contributing, please read [CONTRIBUTING.md](CONTRIBUTING.md)

## Build Pipeline

This package is built in a few different ways:

- Travis (see [`../.travis.yml`](../.travis.yml))
    - Runs `R CMD CHECK`
    - Builds the `pkgdown` site
- Jenkins (see [`../Jenkinsfile`](../Jenkinsfile))
    - Runs the test suite
    - Runs an integration test suite against actual Connect servers

### Jenkins

The Jenkins pipeline is a bit complex, but can be tested locally with the following:

```
# from the root of the repository
export RSC_LICENSE=my-license-key
make test
```

NOTE: this takes a good bit of time because it bootstraps a dev environment. If
you want to use a local dev environment, you can:

```
# from the root of the repository
export RSC_LICENSE=my-license-key
make network-up test-env-up
```

Then you can interact with the RStudio Connect servers directly. Networking is a
little trickier in this setup because you are not inside of the docker network.
