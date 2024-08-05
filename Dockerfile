# Use the official R image as the base image
FROM rocker/r-ver:4.1.0

# Install system dependencies for Shiny Server
RUN apt-get update && apt-get install -y \
    sudo \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libxml2-dev \
    libxt6 \
    wget

# Copy the requirements file
COPY requirements.R /requirements.R

# Install R packages
RUN R -f /requirements.R

# Download and install Shiny Server
RUN wget --quiet https://download3.rstudio.org/ubuntu-14.04/x86_64/shiny-server-1.5.15.953-amd64.deb && \
    dpkg -i shiny-server-1.5.15.953-amd64.deb && \
    rm shiny-server-1.5.15.953-amd64.deb

# Copy the entire Shiny app directory to the image
COPY . /srv/shiny-server/

# Make all app files readable (required by shiny)
RUN chmod -R 755 /srv/shiny-server/

# Expose the port Shiny Server listens on
EXPOSE 3838

# Run Shiny Server
CMD ["/usr/bin/shiny-server.sh"]
