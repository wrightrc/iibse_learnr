FROM rocker/shiny:latest
LABEL maintainer="wrightrc@vt.edu"

ENV S6_VERSION=v1.21.7.0
ENV SHINY_SERVER_VERSION=latest
ENV PANDOC_VERSION=default

RUN /rocker_scripts/install_shiny_server.sh

# Install Shiny Dependencies
RUN install2.r -e -s \
               remotes \
               rmarkdown \
#               learnr \
               sortable

RUN Rscript -e 'remotes::install_github("rundel/learnrhash")'
RUN Rscript -e 'remotes::install_github("rstudio/learnr")'
RUN Rscript -e 'remotes::install_github("wrightrc/interactiveBSE")'

# Copy Shiny App to Container
COPY inst/tutorials /srv/shiny-server

# Give user 'shiny' write access to server
RUN chown -R shiny:shiny /srv/shiny-server

# Open Shiny Port
EXPOSE 3838

CMD ["/init"]
