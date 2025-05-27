FROM r-base:4.5.0

# Configure apt and install software dependencies
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
    apt-transport-https \
    ca-certificates \
    gnupg \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# Install build essentials and basic tools
RUN apt-get update && \
    DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-recommends \
    cmake \
    g++ \
    git \
    wget \
    make \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# Install development tools
RUN apt-get update && \
    DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-recommends \
    automake \
    libtool \
    autoconf \
    pkg-config \
    supervisor \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

ENV TZ=Etc/UTC

# Install scientific and development libraries
RUN apt-get update && \
    DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-recommends \
    libnetcdf-dev \
    libcurl4-openssl-dev \
    libcpprest-dev \
    doxygen \
    graphviz \
    libsqlite3-dev \
    libboost-all-dev \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# Install GDAL dependencies
RUN apt-get update && \
    DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-recommends \
    libproj-dev \
    libgdal-dev \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# install other necessary packages
RUN apt-get update && \
    DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-recommends \
    libsodium-dev \
    libudunits2-dev \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# Install devtools package
RUN R -e "install.packages('devtools')"

# Install gdalcubes package
RUN R -e "install.packages('gdalcubes')"

RUN Rscript -e "install.packages(c('plumber', 'useful', 'ids', 'R6', 'sf', 'rstac','bfast','geojsonsf'))"

# create directories
RUN mkdir -p /opt/dockerfiles/ && mkdir -p /var/openeo/workspace/ && mkdir -p /var/openeo/workspace/data/

# install packages from local directory
COPY ./ /opt/dockerfiles/
# Verbose installation to see any errors
RUN Rscript -e "options(warn=2); message('Installing openeocubes package...'); remotes::install_local('/opt/dockerfiles', dependencies = TRUE, force = TRUE, verbose = TRUE); message('Checking if package is installed:'); installed.packages()[,'Package']"

# Make sure R can find the package
RUN R -e "message('.libPaths(): ', paste(.libPaths(), collapse=', ')); if(!requireNamespace('openeocubes', quietly = TRUE)) stop('openeocubes package not installed!')"

# cmd or entrypoint for startup
CMD ["R", "-q", "--no-save", "-f /opt/dockerfiles/startProduction.R"]

EXPOSE 8000