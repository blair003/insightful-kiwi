Sys.setenv(R_LIBS_USER="~/.R/library")
.libPaths("~/.R/library")

if (!require("unmarked", quietly = TRUE)) {
  install.packages("unmarked", repos = "https://cloud.r-project.org", lib="~/.R/library")
}
