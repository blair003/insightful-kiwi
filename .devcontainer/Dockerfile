FROM rocker/shiny-verse:4.5.3

ENV DEBIAN_FRONTEND=noninteractive
ENV TZ=Pacific/Auckland
# ENV PATH="/opt/venv/bin:$PATH"

# First install tools needed to add external apt repos
RUN apt-get update && apt-get install -y --no-install-recommends \
    gnupg \
    wget \
    ca-certificates \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# Add Google Chrome repo
RUN wget -q -O - https://dl.google.com/linux/linux_signing_key.pub \
    | gpg --dearmor \
    > /usr/share/keyrings/google-linux-signing-key.gpg && \
    echo "deb [arch=amd64 signed-by=/usr/share/keyrings/google-linux-signing-key.gpg] https://dl.google.com/linux/chrome/deb/ stable main" \
    > /etc/apt/sources.list.d/google-chrome.list

# Main system dependencies
RUN apt-get update && apt-get install -y --no-install-recommends \
    libmagick++-dev \
    libmagickwand-dev \
    libudunits2-dev \
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    libsqlite3-dev \
    libpango-1.0-0 \
    libcairo2 \
    libcairo2-dev \
    libffi-dev \
    libpango1.0-dev \
    libpangocairo-1.0-0 \
    libfreetype6 \
    libfreetype6-dev \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    cmake \
    gdal-bin \
    libjq-dev \
    libnode-dev \
    python3-pip \
    python3-cffi \
    python3-cairo \
    python3-venv \
    curl \
    unzip \
    git \
    openssh-client \
    bubblewrap \
    pandoc \
    google-chrome-stable \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*
    
# WeasyPrint
RUN python3 -m venv /opt/venv && \
    /opt/venv/bin/pip install --no-cache-dir --upgrade pip && \
    /opt/venv/bin/pip install --no-cache-dir weasyprint && \
    ln -s /opt/venv/bin/weasyprint /usr/local/bin/weasyprint

ENV PKG_SYSREQS=false

# R packages
COPY .devcontainer/setup-r.R /tmp/setup-r.R
RUN Rscript /tmp/setup-r.R

# Shiny Server defaults to /srv/shiny-server
WORKDIR /srv/shiny-server

EXPOSE 3838

# Do not override CMD.
# rocker/shiny-verse already starts shiny-server.