# Install R version 3.6.3
FROM rocker/shiny:3.6.3

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
  unixodbc-dev \
  libgdal-dev \
  libudunits2-dev \
  && install2.r --error \
    --deps TRUE \
    tidyverse \
    dplyr \
    devtools \
    formatR \
    remotes \
    selectr \
    caTools \
    BiocManager \
  && rm -rf /tmp/downloaded_packages 

# Install R packages that are required
 # RUN R -e "install.packages('shiny', repos='http://cran.rstudio.com/')"
 # RUN R -e "install.packages('tidyverse', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('lubridate', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('glue', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('raster', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('ggridges', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('shinydashboard', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('shinyWidgets', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('splitstackshape', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('RColorBrewer', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('measurements', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('sf', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('leaflet', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('DT', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('ggnewscale', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('cowplot', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('ncdf4', repos='http://cran.rstudio.com/')"
 # RUN R -e "install.packages('rnoaa', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('ggpubr', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('vegan', repos='http://cran.rstudio.com/')"
 # RUN R -e "install.packages('feather', repos='http://cran.rstudio.com/')"
 # RUN R -e "install.packages('rsconnect', repos='http://cran.rstudio.com/')"

# EXPOSE 3838

COPY /kfmapp /srv/shiny-server/

COPY shiny-server.sh /usr/bin/shiny-server.sh

CMD ["/usr/bin/shiny-server.sh"]
