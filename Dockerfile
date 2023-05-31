FROM ubuntu:jammy

RUN apt-get update && DEBIAN_FRONTEND=noninteractive apt-get install -y software-properties-common cmake g++ git supervisor wget
ENV TZ=Etc/UTC
RUN apt-get install  -y libnetcdf-dev libcurl4-openssl-dev libcpprest-dev doxygen graphviz  libsqlite3-dev libboost-all-dev
RUN add-apt-repository -y ppa:ubuntugis/ubuntugis-unstable && apt-key adv --keyserver keyserver.ubuntu.com --recv-keys 314DF160 && apt-get update && apt-get install -y libproj-dev libgdal-dev

# add key for R repository
RUN apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E298A3A825C0D65DFD57CBB651716619E084DAB9

# add R repository
RUN add-apt-repository 'deb https://cloud.r-project.org/bin/linux/ubuntu jammy-cran40/'

# install R
RUN apt update -q && DEBIAN_FRONTEND=noninteractive apt install -q -y r-base

# install devtools package
RUN Rscript -e "install.packages('devtools')"

# install gdalcubes package
RUN Rscript -e "install.packages('gdalcubes')"

# install other necessary packages
RUN apt-get install -y libsodium-dev libudunits2-dev
RUN Rscript -e "install.packages(c('plumber', 'useful', 'ids', 'R6', 'sf', 'rstac','bfast', 'caret', 'randomForest' ,'xgboost', 'torch' , 'sits'))"

# create directories
RUN mkdir -p /opt/dockerfiles/ && mkdir -p /var/openeo/workspace/ && mkdir -p /var/openeo/workspace/data/

# install packages from local directory
COPY ./ /opt/dockerfiles/
RUN Rscript -e "remotes::install_local('/opt/dockerfiles',dependencies=TRUE)"

# cmd or entrypoint for startup
CMD ["R", "-q", "--no-save", "-f /opt/dockerfiles/Dockerfiles/start.R"]

EXPOSE 8000