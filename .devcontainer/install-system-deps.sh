#!/usr/bin/env bash
set -euo pipefail

export DEBIAN_FRONTEND=noninteractive
export TZ=Pacific/Auckland

apt-get update
apt-get install -y --no-install-recommends \
    gnupg \
    wget \
    ca-certificates

wget -q -O - https://dl.google.com/linux/linux_signing_key.pub \
    | gpg --dearmor \
    > /usr/share/keyrings/google-linux-signing-key.gpg

echo "deb [arch=amd64 signed-by=/usr/share/keyrings/google-linux-signing-key.gpg] https://dl.google.com/linux/chrome/deb/ stable main" \
    > /etc/apt/sources.list.d/google-chrome.list

apt-get update
apt-get install -y --no-install-recommends \
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
    google-chrome-stable

python3 -m venv /opt/venv
/opt/venv/bin/pip install --no-cache-dir --upgrade pip
/opt/venv/bin/pip install --no-cache-dir weasyprint
ln -sf /opt/venv/bin/weasyprint /usr/local/bin/weasyprint

apt-get clean
rm -rf /var/lib/apt/lists/*
