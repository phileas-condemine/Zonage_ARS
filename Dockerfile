FROM rocker/tidyverse:latest
RUN apt-get update -qq && apt-get -y --no-install-recommends install \
libudunits2-dev \
libgdal-dev \
gdal-bin \
libgeos-dev \
libproj-dev \
libpoppler-cpp-dev
WORKDIR /home
RUN git clone https://github.com/phileas-condemine/Zonage_ARS.git
WORKDIR ./Zonage_ARS
RUN Rscript install_deps.R
RUN R -e "webshot::install_phantomjs()"
