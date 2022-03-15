# combine the modeled FF predictions

# turn off scipen
options(scipen = 999)

# Libraries ---------------------------------------------------------------

library(tidyverse)
library(glue)
library(fs)

# Read in and compile data ------------------------------------------------

# map approach
filepaths <- fs::dir_ls("model_output/CA_NHD_FFMs/", glob = "*.csv")
(filenames <- gsub(path_file(filepaths), pattern = "_nhd.csv", replacement = ""))

stopifnot(length(filenames)==length(filepaths))  # must be same
dat <- map2_df(filepaths, filenames, ~read_csv(.x) %>% mutate(metric = .y))

# read_csv approach pretty seamless
#dat2 <- read_csv(filepaths, id="path")


# Summarize to Single Value per COMID -------------------------------------

# collapse data by comid and year: try median

dat_med <- dat %>%
  group_by(comid, metric) %>%
  summarize(across(.cols = c(p10, p25, p50, p75, p90), mean, na.rm=TRUE))

summary(dat_med)
table(dat_med$metric)
