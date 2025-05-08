FROM r-base:4.3.0

# Install software dependencies
RUN apt-get update && DEBIAN_FRONTEND=noninteractive apt-get install -y \
    software-properties-common \
    cmake \
    g++ \
    git \
    supervisor \
    wget \
    curl \
    libnetcdf-dev \
    libcurl4-openssl-dev \
    libcpprest-dev \
    doxygen \
    graphviz \
    libsqlite3-dev \
    libboost-all-dev \
    libproj-dev \
    libgdal-dev \
    libsodium-dev \
    libudunits2-dev

ENV TZ=Etc/UTC

# Install R packages
RUN R -e "install.packages(c('devtools', 'remotes', 'roxygen2', 'testthat', 'knitr', 'rmarkdown'))"
RUN R -e "install.packages(c('gdalcubes', 'plumber', 'useful', 'ids', 'R6', 'sf', 'rstac', 'bfast', 'dplyr', 'base64enc', 'jsonlite', 'tibble', 'rlang', 'tools'))"

# create directories
RUN mkdir -p /opt/dockerfiles/ && \
    mkdir -p /var/openeo/workspace/ && \
    mkdir -p /var/openeo/workspace/data/

# install packages from local directory
COPY ./ /opt/dockerfiles/


# Install the package 
RUN Rscript -e "remotes::install_local('/opt/dockerfiles',dependencies=TRUE)"


# cmd or entrypoint for startup
CMD ["R", "-q", "--no-save", "-f /opt/dockerfiles/startProduction.R"]

EXPOSE 8000