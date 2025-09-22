# Use the official R Shiny image as base
FROM rocker/shiny:4.3.0

# Set maintainer
LABEL maintainer="your-email@example.com"

# Install system dependencies for geospatial packages
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    libudunits2-dev \
    libnode-dev \
    libcairo2-dev \
    libnetcdf-dev \
    netcdf-bin \
    && rm -rf /var/lib/apt/lists/*

# Install R packages
RUN R -e "install.packages(c('remotes', 'devtools'), repos='https://cran.rstudio.com/')"

# Install packages from CRAN
RUN R -e "install.packages(c( \
    'shiny', \
    'shinythemes', \
    'shinyWidgets', \
    'DT', \
    'leaflet', \
    'raster', \
    'rgdal', \
    'geojsonio', \
    'stringdist', \
    'rmapshaper', \
    'shinyjs' \
    ), repos='https://cran.rstudio.com/')"

# Install RavenR from GitHub (assuming it's not on CRAN)
RUN R -e "remotes::install_github('rchlumsk/RavenR')"

# Create app directory
RUN mkdir /srv/shiny-server/geojson-converter

# Copy the Shiny app files
COPY app.R /srv/shiny-server/geojson-converter/

# Create www directory for static files (if you have the banner image)
RUN mkdir -p /srv/shiny-server/geojson-converter/www

# If you have the banner image, copy it to www directory
# COPY ravenbanner180.png /srv/shiny-server/geojson-converter/www/

# Set permissions
RUN chown -R shiny:shiny /srv/shiny-server/geojson-converter

# Create a startup script to handle permissions and start shiny-server
RUN echo '#!/bin/bash\n\
chown -R shiny:shiny /srv/shiny-server\n\
exec shiny-server' > /usr/local/bin/start.sh && \
chmod +x /usr/local/bin/start.sh

# Expose port
EXPOSE 3838

# Set working directory
WORKDIR /srv/shiny-server/geojson-converter

# Start the application
CMD ["/usr/local/bin/start.sh"]