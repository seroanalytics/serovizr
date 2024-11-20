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
        base64enc \
        cachem \
        callr \
        docopt \
        fs \
        ggplot2 \
        logger \
        lubridate \
        jsonlite \
        jsonvalidate \
        plotly \
        plumber \
        redoc \
        remotes \
        Rook \
        stringr \
        tibble \
        yaml

RUN Rscript -e "install.packages('remotes')"
RUN Rscript -e 'remotes::install_github("hillalex/porcelain@i39")'

WORKDIR /serovizr

COPY . /src

RUN R CMD INSTALL /src

EXPOSE 80

ENTRYPOINT ["/usr/local/bin/seroviz", "--port=80"]
