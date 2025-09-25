FROM r-base:4.5.0

ENV DEBIAN_FRONTEND=noninteractive
ENV TZ=Etc/UTC
ENV CXXFLAGS="-O2 -pipe -std=gnu++17"

RUN apt-get update && apt-get install -y --no-install-recommends \
    ca-certificates gnupg apt-transport-https \
    build-essential cmake g++ git wget make \
    automake libtool autoconf pkg-config supervisor \
    ninja-build zip unzip \
    && apt-get clean && rm -rf /var/lib/apt/lists/*

RUN apt-get update && apt-get install -y --no-install-recommends \
    libproj-dev libgdal-dev libgeos-dev \
    libnetcdf-dev libcurl4-openssl-dev libcpprest-dev \
    libsqlite3-dev libboost-all-dev \
    libsodium-dev libudunits2-dev \
    zlib1g-dev libunwind-dev libssl-dev libxml2-dev \
    doxygen graphviz \
    && apt-get clean && rm -rf /var/lib/apt/lists/*

RUN git clone --depth 1 --branch 20230802.1 https://github.com/abseil/abseil-cpp.git /tmp/abseil && \
    cmake -S /tmp/abseil -B /tmp/abseil/build \
      -DCMAKE_BUILD_TYPE=Release \
      -DCMAKE_POSITION_INDEPENDENT_CODE=ON \
      -DABSL_PROPAGATE_CXX_STD=ON \
      -DABSL_ENABLE_INSTALL=ON && \
    cmake --build /tmp/abseil/build -j"$(nproc)" && \
    cmake --install /tmp/abseil/build && \
    rm -rf /tmp/abseil

ENV PKG_CONFIG_PATH="/usr/local/lib/pkgconfig${PKG_CONFIG_PATH:+:$PKG_CONFIG_PATH}"

RUN R -e "install.packages(c('remotes'), repos='https://cloud.r-project.org')"
RUN R -e "install.packages(c('devtools'), repos='https://cloud.r-project.org')"

RUN R -e "install.packages(c('gdalcubes','plumber','useful','ids','R6','s2','sf','rstac','bfast','geojsonsf'), repos='https://cloud.r-project.org')"

RUN mkdir -p /opt/dockerfiles/ /var/openeo/workspace/ /var/openeo/workspace/data/

COPY ./ /opt/dockerfiles/
RUN Rscript -e "options(warn=2); \
  message('Installing openeocubes package...'); \
  remotes::install_local('/opt/dockerfiles', dependencies = TRUE, force = TRUE, verbose = TRUE); \
  message('Checking if package is installed:'); \
  if(!('openeocubes' %in% rownames(installed.packages()))) stop('openeocubes package not installed!')"

RUN R -e "message('.libPaths(): ', paste(.libPaths(), collapse=', ')); \
  if(!requireNamespace('openeocubes', quietly = TRUE)) stop('openeocubes package not installed!')"

EXPOSE 8000
CMD ["R", "-q", "--no-save", "-f", "/opt/dockerfiles/startProduction.R"]
