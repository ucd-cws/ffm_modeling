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

# save out:
write_csv(dat_med, "model_output/updated_lshasta_ffm_preds_means.csv")


# Notes -------------------------------------------------------------------

# look at comid 3917950 as it's where the canal joined

# In comparing the two, there are many potential issues:

# FA_Timing is very wrong, only P10 is even possible, the other values look like magnitudes
# Peak 2, 5, 10: All have very low values...possible but unlikely
# SP_Dur: the p75 and p90 seem very high (P90 is ~ 6 months)
# SP_Mag: appears low, possible but unlikely if this is predicted
# Wet_Tim: P50 is 30 days more than the original estimate, p75/p90 are very very high (~6 months)
