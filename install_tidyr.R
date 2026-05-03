Sys.setenv(R_LIBS_USER="~/.R/library")
.libPaths("~/.R/library")
if (!require("tidyr", quietly = TRUE)) {
  install.packages("tidyr", repos = "https://cloud.r-project.org", lib="~/.R/library")
}
