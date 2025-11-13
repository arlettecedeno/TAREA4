FROM rocker/shiny:latest

# Instalar librerias
RUN install2.r --error \
    readr \
    dplyr \
    tidyr \
    ggplot2 \
    GGally \
    reshape2 \
    arrow

# Toma el archivo shiny
COPY SHINY.R /srv/shiny-server/app.R

# Toma el archivo feather
COPY boston_clean.feather /srv/shiny-server/boston_clean.feather

# Puerto interno del servidor Shiny
EXPOSE 3838

CMD ["/usr/bin/shiny-server"]
