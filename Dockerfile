FROM r-base:4.3.0

# Install software dependencies
RUN apt-get update && DEBIAN_FRONTEND=noninteractive apt-get install -y software-properties-common cmake g++ git supervisor wget
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
RUN Rscript -e "remotes::install_local('/opt/dockerfiles',dependencies=TRUE)"

# cmd or entrypoint for startup
CMD ["R", "-q", "--no-save", "-f /opt/dockerfiles/startProduction.R"]

EXPOSE 8000