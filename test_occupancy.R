Sys.setenv(R_LIBS_USER="~/.R/library")
.libPaths("~/.R/library")

if (!require("unmarked", quietly = TRUE)) {
  install.packages("unmarked", repos = "https://cloud.r-project.org", lib="~/.R/library")
}
library(unmarked)

# dummy data
y <- matrix(c(1,0,1, 0,0,0, 1,1,1, 0,NA,NA), nrow=4, byrow=TRUE)
umf <- unmarkedFrameOccu(y = y)
fm <- occu(~1 ~1, umf)
summary(fm)
est <- predict(fm, type="state")
print(est)
