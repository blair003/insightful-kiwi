#!/usr/bin/env bash
set -e

ssh -t sitehost '
  cd /opt/docker/services/shiny-server/data/wkt.insightful.kiwi &&
  git pull --ff-only &&
  cd /opt/docker/services/shiny-server &&
  sudo docker compose restart shiny
'
