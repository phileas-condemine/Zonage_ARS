FROM rocker/rstudio:latest
RUN apt-get update -qq && apt-get -y --no-install-recommends install \
libudunits2-dev \
libgdal-dev \
gdal-bin \
libgeos-dev \
libproj-dev 
RUN git clone https://github.com/phileas-condemine/Zonage_ARS.git
RUN Rscript Zonage_ARS/install_deps.R