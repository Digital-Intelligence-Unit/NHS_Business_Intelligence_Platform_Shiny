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
  unixodbc-dev

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
    rvest\
    visNetwork\
    jsonlite\
    forcat\
    && rm -rf /tmp/downloaded_packages
