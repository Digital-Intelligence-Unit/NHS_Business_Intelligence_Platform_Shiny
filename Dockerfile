FROM rocker/shiny

ARG PGDATABASE
ENV PGDATABASE ${PGDATABASE}
ARG PGPORT=5432
ENV PGPORT ${PGPORT}
ARG AWSPROFILE
ENV AWSPROFILE ${AWSPROFILE}
ARG JWT_SECRET
ENV JWT_SECRET ${JWT_SECRET}
ARG JWT_SECRETKEY
ENV JWT_SECRETKEY ${JWT_SECRETKEY}
ARG POSTGRES_UN
ENV POSTGRES_UN ${POSTGRES_UN}
ARG POSTGRES_PW
ENV POSTGRES_PW ${POSTGRES_PW}
ARG AWS_SECRETID
ENV AWS_SECRETID ${AWS_SECRETID}
ARG AWS_SECRETKEY
ENV AWS_SECRETKEY ${AWS_SECRETKEY}
ENV API_NAME=biplatform-shiny, AWSREGION=eu-west-2

RUN apt-get update && apt-get install -y \
    --no-install-recommends \
    sudo \
    gdebi-core \
    pandoc \
    pandoc-citeproc \
    libcairo2-dev \
    libxt-dev \
    libpq-dev \
    libssl-dev \
    libcurl4-openssl-dev \
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
    unixodbc-dev \
    r-cran-rcpp \
    r-cran-inline \
    r-cran-rcpp \
    r-cran-rstan \
    build-essential \
    g++ 

RUN sudo apt install -y libmariadb-dev

# Run this to install prophet package
# install BH otherwise it would complain with the errors
RUN R -e "install.packages('https://cran.r-project.org/src/contrib/Archive/BH/BH_1.62.0-1.tar.gz')"
# install remotes to pull rstan from the source
RUN R -e "install.packages('remotes', type = 'source')"
RUN R -e "library('remotes')"
RUN R -e "install.packages('rstan', type = 'source')"
# somehow prophet installs correctly (ignore all the red things)
RUN R -e "library('rstan')"
RUN R -e "install.packages('prophet', type='source')"
RUN R -e "library('prophet')"

RUN R -e "install.packages(\
  c(\
    'shiny',\
    'rmarkdown',\
    'ggplot2',\
    'plotly',\
    'dplyr',\
    'stringr',\
    'devtools',\
    'formatR',\
    'remotes',\
    'selectr',\
    'caTools',\
    'config',\
    'RPostgreSQL',\
    'DT',\
    'readxl',\
    'shinyWidgets',\
    'shinyjs',\
    'BiocManager',\
    'DBI',\
    'odbc',\
    'shinydashboard',\
    'shinyBS',\
    'treemap',\
    'RColorBrewer',\
    'shinyTree',\
    'scales',\
    'cluster',\
    'FactoMineR',\
    'table',\
    'epiDisplay',\
    'bnlearn',\
    'bnviewer',\
    'fastcluster',\
    'gridExtra',\
    'factoextra',\
    'kmed',\
    'Rtsne',\
    'umap',\
    'rpart',\
    'plot',\
    'rattle',\
    'sparkline',\
    'xml2',\
    'rvest',\
    'visNetwork',\
    'jsonlite',\
    'forcats',\
    'shinythemes',\
    'tidyverse',\
    'NHSRplotthedots',\
    'runcharter',\
    'qicharts2',\
    'forecast',\
    'lubridate'\
  ),\
  repos = 'https://cran.rstudio.com/'\
)" && \
  cp -R /usr/local/lib/R/site-library/shiny/examples/* /srv/shiny-server/ && \
  chown shiny:shiny /var/lib/shiny-server

COPY mountpoints/apps/ /srv/shiny-server/

ENV SHINY_LOG_STDERR=1
ENV SHINY_LOG_LEVEL=TRACE

EXPOSE 3838

COPY shiny-server.sh /usr/bin/shiny-server.sh
RUN ["chmod", "+x", "/usr/bin/shiny-server.sh"]
ENTRYPOINT ["sh", "/usr/bin/shiny-server.sh"]


