FROM rocker/shiny:4.0.2

RUN apt-get update -qq && apt-get -y --no-install-recommends install \
  libxml2-dev \
  libcairo2-dev \
  libsqlite3-dev \
  libmariadbd-dev \
  libmariadbclient-dev \
  libpq-dev \
  libssl-dev \
  libcurl4-openssl-dev \
  libssh2-1-dev \
  libxml2-dev \
  unixodbc-dev \
  libfontconfig1-dev \
  libharfbuzz-dev \ 
  libfribidi-dev \
  libfreetype6-dev \
  libpng-dev \
  libtiff5-dev \
  libjpeg-dev \
  r-cran-rcpp \
  r-cran-inline \
  r-cran-rcpp \
  build-essential

COPY ../installer.R ./
RUN Rscript installer.R

RUN install2.r --error  --skipinstalled \
    --deps TRUE \
    shiny\
    rmarkdown\
    tidyverse\
    ggplot2\
    plotly\
    dplyr\
    stringr\
    devtools\
    formatR\
    remotes\
    selectr\
    caTools\
    config\
    RPostgreSQL\
    DT\
    readxl\
    shinyWidgets\
    shinyjs\
    BiocManager\
    DBI\
    odbc\
    shinydashboard\
    shinyBS\
    treemap\
    RColorBrewer\
    shinyTree\
    scales\
    cluster\
    FactoMineR\
    table\
    epiDisplay\
    bnlearn\
    bnviewer\
    fastcluster\
    gridExtra\
    factoextra\
    kmed\
    Rtsne\
    umap\
    rpart\
    plot\
    rattle\
    xml2\
    rvest\
    visNetwork\
    jsonlite\
    forcat\
    shinythemes\
    tidyverse\
    NHSRplotthedots\
    runcharter\
    qicharts2\
    forecast\
    lubridate\
    prophet\
    && rm -rf /tmp/downloaded_packages
