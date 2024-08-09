FROM rocker/r-ver:latest

RUN apt-get update && apt-get install -y --no-install-recommends \
        libcurl4-openssl-dev \
        libv8-dev \
        libglpk-dev \
        libsodium-dev \
        && apt-get clean \
        && rm -rf /var/lib/apt/lists/*

COPY docker/bin /usr/local/bin/

RUN install_packages --repo=https://mrc-ide.r-universe.dev \
        callr \
        docopt \
        logger \
        jsonlite \
        jsonvalidate \
        plumber \
        porcelain \
        remotes \
        rlang

WORKDIR /serovizr

COPY . /src

RUN R CMD INSTALL /src

EXPOSE 8888

ENTRYPOINT ["/usr/local/bin/seroviz"]
