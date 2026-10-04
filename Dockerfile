# ==============================================================================
# Dockerfile for Hospital Patient Analytics Dashboard (Render Deployment)
# ==============================================================================
FROM rocker/shiny-verse:latest

# Maintainer / Metadata
LABEL maintainer="Healthcare Data Analytics"
LABEL description="Hospital Patient Analytics Dashboard using R & Shiny"

# Install system dependencies required for openxlsx, caret, and spatial/math libraries
RUN apt-get update && apt-get install -y --no-install-recommends \
    libzip-dev \
    libglpk-dev \
    libxml2-dev \
    libssl-dev \
    libcurl4-openssl-dev \
    && rm -rf /var/lib/apt/lists/*

# Configure Posit Package Manager (P3M) binary repository for lightning-fast package install
# This installs pre-compiled Linux binaries in seconds rather than compiling from source
RUN echo "options(repos = c(CRAN = 'https://packagemanager.posit.co/cran/__linux__/jammy/latest'))" >> /usr/local/lib/R/etc/Rprofile.site

# Install R packages needed by the dashboard
# Note: shiny, dplyr, ggplot2, readr, lubridate, tidyr are already pre-installed in rocker/shiny-verse!
RUN R -e "install.packages(c('bslib', 'plotly', 'DT', 'openxlsx', 'caret', 'randomForest'))"

# Set working directory in container
WORKDIR /app

# Copy all application files into the container
COPY . /app

# Grant read/write permissions for runtime model persistence and temporary files
RUN chmod -R 777 /app

# Default port for Render (Render dynamically sets the $PORT environment variable)
ENV PORT=10000
ENV HOST=0.0.0.0

# Expose Render standard port
EXPOSE 10000

# Launch application using run.R
CMD ["Rscript", "run.R"]
