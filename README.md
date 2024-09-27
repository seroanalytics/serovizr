# serovizr

<!-- badges: start -->
[![Project Status: WIP – Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![R-CMD-check.yaml](https://github.com/seroanalytics/serovizr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/seroanalytics/serovizr/actions/workflows/R-CMD-check.yaml)
[![codecov](https://codecov.io/gh/seroanalytics/serovizr/graph/badge.svg?token=oFACWrbYep)](https://codecov.io/gh/seroanalytics/serovizr)
![Docker Image Version](https://img.shields.io/docker/v/seroanalytics/serovizr?logo=docker)
![GitHub License](https://img.shields.io/github/license/seroanalytics/serovizr)
<!-- badges: end -->

R API for the SeroViz app. Based on the [porcelain](https://github.com/reside-ic/porcelain) and [plumber](https://github.com/rstudio/plumber) frameworks.

## API Specification
Docs are available when running the API locally on port 8888, via 
```
http://127.0.0.1:8888/__docs__/
```

The easiest way to run the API locally is via Docker:

``` 
 docker run -p 8888:8888 seroanalytics/serovizr:main
```

Alternatively, to run from R, first clone this repo and then from this directory run:

```r
  devtools::load_all()
  serovizr:::main()
```

The docs are maintained via an [openapi](https://www.openapis.org/) specification
contained in `inst/spec.yaml`, and [JSON Schema](https://json-schema.org/) files in `inst/schema`.

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

``` 
./docker/build
```

To push to Dockerhub:

``` 
./docker/push
```


To run a built image:

``` 
 docker run -p 8888:8888 seroanalytics/serovizr:<branch-name>
```

These steps are run on CI.

For a complete list of available tags, see Dockerhub: 
https://hub.docker.com/repository/docker/seroanalytics/serovizr/tags

The API is deployed along with the SeroViz app itself; see:
https://github.com/seroanalytics/seroviz?tab=readme-ov-file#deployment
