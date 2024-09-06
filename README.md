# serovizr

<!-- badges: start -->
[![Project Status: WIP – Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![R-CMD-check.yaml](https://github.com/seroanalytics/serovizr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/seroanalytics/serovizr/actions/workflows/R-CMD-check.yaml)
[![codecov](https://codecov.io/gh/seroanalytics/serovizr/graph/badge.svg?token=oFACWrbYep)](https://codecov.io/gh/seroanalytics/serovizr)
![Docker Image Version](https://img.shields.io/docker/v/seroanalytics/serovizr?logo=docker)
![GitHub License](https://img.shields.io/github/license/seroanalytics/serovizr)
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

To push to Dockerhub:

``` r
./docker/push
```


To run a built image:

``` r
 docker run -p 8888:8888 seroanalytics/serovizr:<branch-name>
```

These steps are run on CI.

For a complete list of available tags, see Dockerhub: 
https://hub.docker.com/repository/docker/seroanalytics/serovizr/tags

The API is deployed along with the SeroViz app itself; see:
https://github.com/seroanalytics/seroviz?tab=readme-ov-file#deployment
