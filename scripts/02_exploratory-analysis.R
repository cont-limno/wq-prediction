
# ---- load_packages-and-data ----

library(dplyr)
library(tibble)

# flist <- list.files("data", pattern = ".csv",
#                     include.dirs = TRUE, full.names = TRUE)
# dt <- lapply(flist, function(x) read.csv(x, stringsAsFactors = FALSE))

flist <- "data/limno_data.csv"
dt    <- read.csv(flist, stringsAsFactors = FALSE)
