# serovizr

<!-- badges: start -->
[![Project Status: Concept – Minimal or no implementation has been done yet, or the repository is only intended to be a limited example, demo, or proof-of-concept.](https://www.repostatus.org/badges/latest/concept.svg)](https://www.repostatus.org/#concept)
[![R build status](https://github.com/seroanalytics/serovizr/workflows/R-CMD-check/badge.svg)](https://github.com/seroanalytics/serovizr/actions)
[![Codecov test coverage](https://codecov.io/gh/seroanalytics/serovizr/branch/master/graph/badge.svg)](https://codecov.io/gh/seroanalytics/serovizr?branch=master)
<!-- badges: end -->

R API for the SeroViz app. Based on the [porcelain](https://github.com/reside-ic/porcelain) framework.

## Developing
Install dependencies with:

```r
remotes::install_deps(".", dependencies = TRUE)
```

Start the API locally by running:

```r
devtools::load_all()
serovizr:::main()
```

## Testing
Run tests with:

```r
devtools::test()
```

## Deploying
To build a Docker image:

``` r
./docker/build
```

To run a built image:

``` r
 docker run -p 8888:8888 seroanalytics/serovizr:<branch-name>
```
