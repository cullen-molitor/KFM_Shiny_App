FROM rocker/shiny:4.0.3

LABEL org.label-schema.license="GPL-2.0" \
      org.label-schema.vcs-url="https://github.com/rocker-org/rocker-versioned" \
      org.label-schema.vendor="Rocker Project" \
      maintainer="Carl Boettiger <cboettig@ropensci.org>"



RUN /rocker_scripts/install_tidyverse.sh
RUN /rocker_scripts/install_geospatial.sh

# Install R packages that are required
RUN R -e "install.packages('shinydashboard')"
RUN R -e "install.packages('shinyWidgets')"
RUN R -e "install.packages('plotly')"
RUN R -e "install.packages('ggpubr')"
RUN R -e "install.packages('glue')"
RUN R -e "install.packages('lubridate')"
RUN R -e "install.packages('scales')"
RUN R -e "install.packages('leaflet')"
RUN R -e "install.packages('DT')"
RUN R -e "install.packages('pdp')"
RUN R -e "install.packages('randomForest')"
RUN R -e "install.packages('wordcloud')"
RUN R -e "install.packages('arrow')"
RUN R -e "install.packages('cachem')"
RUN R -e "install.packages('shiny')"
RUN R -e "install.packages('shinycssloaders')"

COPY --chown=shiny:shiny /App /srv/shiny-server/