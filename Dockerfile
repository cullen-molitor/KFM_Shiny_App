FROM rocker/r-ver:4.1.1

LABEL org.opencontainers.image.licenses="GPL-2.0-or-later" \
      org.opencontainers.image.source="https://github.com/rocker-org/rocker-versioned2" \
      org.opencontainers.image.vendor="Rocker Project" \
      org.opencontainers.image.authors="Carl Boettiger <cboettig@ropensci.org>"

ENV S6_VERSION=v2.1.0.2
ENV SHINY_SERVER_VERSION=latest
ENV PANDOC_VERSION=default

RUN /rocker_scripts/install_shiny_server.sh
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
# RUN R -e "install.packages('leaflet')"
RUN R -e "install.packages('DT')"
RUN R -e "install.packages('pdp')"
RUN R -e "install.packages('randomForest')"
RUN R -e "install.packages('wordcloud')"
RUN R -e "install.packages('arrow')"
RUN R -e "install.packages('cachem')"
RUN R -e "install.packages('shiny')"
RUN R -e "install.packages('shinycssloaders')"

COPY --chown=shiny:shiny /App /srv/shiny-server/

EXPOSE 3838

CMD ["/init"]