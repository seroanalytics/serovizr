FROM rocker/r-ver:latest

RUN apt-get update && apt-get install -y --no-install-recommends \
        libcurl4-openssl-dev \
        libv8-dev \
        libglpk-dev \
        libsodium-dev \
        && apt-get clean \
        && rm -rf /var/lib/apt/lists/*

RUN install_packages --repo=https://mrc-ide.r-universe.dev \
        callr \
        docopt \
        geojsonio \
        glue \
        jsonlite \
        jsonvalidate \
        plumber \
        porcelain \
        ps \
        remotes \
        rlang

COPY docker/bin /usr/local/bin/

EXPOSE 8888

ENTRYPOINT ["/usr/local/bin/seroviz"]
