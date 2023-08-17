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

# Run this to install prophet package
# clear up compiler.. looks like it's not working the way it should work but somehow it all works at the end
RUN R -e "dotR <- file.path(Sys.getenv('HOME'), '.R') \
if (!file.exists(dotR)) dir.create(dotR) \
M <- file.path(dotR, 'Makevars') \
if (!file.exists(M)) file.create(M) \
cat('\nCXX14FLAGS=-O3 -march=native -mtune=native -fPIC', \
        'CXX14 = g++ -std=c++1y', \ 
	    file = M, sep = '\n', append = TRUE) \
# clear environment
if (file.exists('.RData')) file.remove('.RData') \
# install BH otherwise it would complain with the errors
install.packages('https://cran.r-project.org/src/contrib/Archive/BH/BH_1.62.0-1.tar.gz') \
# install remotes to pull rstan from the source
install.packages('remotes', type = 'source') \
library('remotes') \
remotes::install_github('stan-dev/rstan', ref = 'develop', subdir = 'rstan/rstan', build_opts = '') \
# somehow prophet installs correctly (ignore all the red things)
library('rstan') \
install.packages('prophet', type='source') \
library('prophet')"

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
