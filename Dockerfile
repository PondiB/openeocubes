FROM r-base:4.5.0

# Install software dependencies
RUN apt-get update && DEBIAN_FRONTEND=noninteractive apt-get install -y \
    software-properties-common \
    cmake \
    g++ \
    git \
    supervisor \
    wget \
    automake \
    libtool \
    autoconf \
    pkg-config \
    make
ENV TZ=Etc/UTC
RUN apt-get install  -y libnetcdf-dev libcurl4-openssl-dev libcpprest-dev doxygen graphviz  libsqlite3-dev libboost-all-dev
RUN apt-get update && apt-get install -y libproj-dev libgdal-dev

# Install devtools package
RUN R -e "install.packages('devtools')"

# Install gdalcubes package
RUN R -e "install.packages('gdalcubes')"

# install other necessary packages
RUN apt-get install -y libsodium-dev libudunits2-dev
RUN Rscript -e "install.packages(c('plumber', 'useful', 'ids', 'R6', 'sf', 'rstac','bfast'))"

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