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
  summarize(across(.cols = c(p10, p25, p50, p75, p90), median, na.rm=TRUE))

summary(dat_med)
table(dat_med$metric)

# save out:
write_csv(dat_med, "model_output/updated_lshasta_ffm_preds_median.csv")


# Notes -------------------------------------------------------------------

# LOI-1: COMID 3917946 at confluence
# LOI-2: COMID 3917950, as it's where the canal joined
# LOI-3: COMID 3917198 upstream

# In comparing these, there are many potential issues:

# Peak 2, 5, 10: All have very low values...ranging between 1-50 cfs

# FA_Dur: Inferred
# Peak_Dur_2, 5, 10: Inferred
# Peak_Fre_2, 5, 10: Inferred
# SP_ROC: Inferred


# Wet_BFL_Dur seems high (p90 ranges ~270), but p50 is reasonable (120-140)
# SP_Dur seems high as well (p50  is about 128, max 145, but p90 is >250)
# DS_Tim avg is 289 (Jul 17) # close
# SP_Tim avg is 255 (Jun 13) # month later


# Join and compare
# prev iteration of ffm, filter to LOIs
ffm <- read_csv("model_output/lshasta_ffc_predictions.csv") %>%
  filter(comid %in% c(3917946, 3917950, 3917198))

dat_med_loi <- dat_med %>%
  filter(comid %in% c(3917946, 3917950, 3917198)) %>%
  select(comid, metric, p50_rev=p50)

# join by comid
ffm_updated <- left_join(ffm, dat_med_loi) %>%
  relocate(p50_rev, .after=p50)


# write out
write_csv(ffm_updated, file="model_output/lshasta_ffc_predictions_revised.csv")
