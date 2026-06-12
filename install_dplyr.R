Sys.setenv(R_LIBS_USER="~/.R/library")
.libPaths("~/.R/library")
if (!require("dplyr", quietly = TRUE)) {
  install.packages("dplyr", repos = "https://cloud.r-project.org", lib="~/.R/library")
}
