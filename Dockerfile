FROM rocker/rstudio:latest
RUN apt-get update -qq && apt-get -y --no-install-recommends install \
libudunits2-dev \
libgdal-dev \
gdal-bin \
libgeos-dev \
libproj-dev 
WORKDIR /home/rstudio
RUN git clone https://github.com/phileas-condemine/Zonage_ARS.git
WORKDIR ./Zonage_ARS
RUN Rscript install_deps.R
RUN su rstudio -c 'R -e "webshot::install_phantomjs()"'
COPY droptoken.rds droptoken.rds
COPY credentials.json credentials.json
COPY .secrets .secrets/
RUN mkdir data
RUN mkdir tests/shinytest/snapshot-current
RUN chown -R rstudio:rstudio ../Zonage_ARS
