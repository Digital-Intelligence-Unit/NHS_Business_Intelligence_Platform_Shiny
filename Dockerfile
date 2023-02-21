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
    wget

RUN sudo apt install -y libmariadb-dev

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
    'rattle'\
  ),\
  repos = 'https://www.stats.bris.ac.uk/R/'\
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


