ARG R_VERSION=4.5.3

FROM rocker/shiny-verse:${R_VERSION} AS base

COPY .devcontainer/install-system-deps.sh /tmp/install-system-deps.sh
RUN bash /tmp/install-system-deps.sh

ENV PKG_SYSREQS=false

COPY .devcontainer/setup-r.R /tmp/setup-r.R
RUN Rscript /tmp/setup-r.R

WORKDIR /srv/shiny-server
EXPOSE 3838


FROM base AS dev

RUN Rscript -e 'install.packages(c("languageserver", "httpgd", "styler", "lintr", "cyclocomp")); install.packages("vscDebugger", repos = c("https://manuelhentschel.r-universe.dev", "https://cloud.r-project.org"))'


FROM base AS production


FROM rocker/rstudio:${R_VERSION} AS rstudio

COPY .devcontainer/install-system-deps.sh /tmp/install-system-deps.sh
RUN bash /tmp/install-system-deps.sh

ENV PKG_SYSREQS=false

COPY .devcontainer/setup-r.R /tmp/setup-r.R
RUN Rscript /tmp/setup-r.R

RUN Rscript -e 'install.packages(c("styler", "lintr"))'

WORKDIR /home/rstudio/project
EXPOSE 8787
