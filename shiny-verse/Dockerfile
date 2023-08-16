FROM rocker/shiny:4.0.2

RUN apt-get update -qq && apt-get -y --no-install-recommends install \
  gdebi-core \
  pandoc \
  pandoc-citeproc \
  libcairo2-dev \
  libsqlite3-dev \
  libmariadbd-dev \
  libmariadbclient-dev \
  libxt-dev \
  libpq-dev \
  libssl-dev \
  libcurl4-openssl-dev \
  libssh2-1-dev \
  xtail \
  libxml2-dev \
  wget \
  libfontconfig1-dev \
  libharfbuzz-dev \ 
  libfribidi-dev \
  libfreetype6-dev \
  libpng-dev \
  libtiff5-dev \
  libjpeg-dev \
  unixodbc-dev

RUN sudo apt install -y libmariadb-dev

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
    prophet\
    lubridate\
    && rm -rf /tmp/downloaded_packages
