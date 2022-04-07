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

# read_csv also approach pretty seamless
#dat2 <- read_csv(filepaths, id="path")

# Summarize to Single Value per COMID -------------------------------------

# collapse data by comid and year: try median

dat_med <- dat %>%
  group_by(comid, metric) %>%
  summarize(across(.cols = c(p10, p25, p50, p75, p90), median, na.rm=TRUE))

summary(dat_med)
table(dat_med$metric)

# add area back in for reference
dat_area <- dat %>% select(comid, area) %>% distinct()
dat_med <- left_join(dat_med, dat_area)

# save out:
write_csv(dat_med, "model_output/updated_lshasta_ffm_preds_median.csv")

# save out loi comids only:
lois <- c(3917198, 3917946, 3917950)
dat_med %>% filter(comid %in% lois) %>% write_csv(., "model_output/updated_lshasta_ffm_preds_LOIs_median.csv")

# Notes -------------------------------------------------------------------

# LOI-1: COMID 3917946 at confluence
# LOI-2: COMID 3917950, as it's where the canal joined
# LOI-3: COMID 3917198 upstream

# FA_Dur: Inferred
# Peak_Dur_2, 5, 10: Inferred
# Peak_Fre_2, 5, 10: Inferred
# SP_ROC: Inferred

# Join and compare
# prev iteration of ffm, filter to LOIs
ffm <- read_csv("model_output/lshasta_ffc_predictions.csv") %>%
  filter(comid %in% c(3917946, 3917950, 3917198))

dat_med_loi <- dat_med %>%
  filter(comid %in% lois) %>%
  rename_with(~paste0(.,"_rev"), -c(comid, metric, area))

# join by comid
ffm_updated <- left_join(ffm, dat_med_loi) %>%
  relocate(p50_rev, .after=p50) %>%
  select(-c(starts_with("observed_"), comid_f))


# write out
write_csv(ffm_updated, file="model_output/lshasta_ffc_predictions_revised_LOIs.csv")
