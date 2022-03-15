#### Apply FFM Models to CA NHD Network

#### Version 3.3 updated Mar 10, 2022 by Ryan Peek (rapeek@ucdavis.edu)
#### Version 3.2, updated September 21, 2021 by Ted Grantham (tgrantham@berkeley.edu)

# GETTING ACCUMULATION REVISED DATA INTO R (read from github: https://github.com/ryanpeek/ffm_accumulation/blob/main/data_clean/08_accumulated_all_metrics.csv)
## run: f_import_accumulation_data
# clean and check: 01_check_accumulation_colnames.R
# Split out by comid, unique csv for each comid: 02_split_by_comid_export.R

# Libraries ---------------------------------------------------------------

library(dplyr)
library(readr) # read zipped csv
library(glue) # better pasting of things together
library(fs) # more stable file paths
library(purrr)
library(janitor)

# Source Functions --------------------------------------------------------

source("code/f_run_rf_model.R")
source("code/f_make_ffm_preds.R")
source("code/f_write_ffm_out.R")

# Load Data ---------------------------------------------------------------

## Load data with all FFM observations and associated watershed variables
met <- read_csv("data_input/model_training/met.csv.zip") %>% clean_names() %>%
  # drop these vars
  select(-c("pmpe", "bdmax", "pmax_ws", "tmin_ws", "permh_ws", "pmin_ws"))

# Metrics -----------------------------------------------------------------

# Here is the list of metrics to predict across the stream network:
## MAG metrics:
#metrics_mag <- c("FA_Mag","Wet_BFL_Mag_50","Wet_BFL_Mag_10","SP_Mag","DS_Mag_90","DS_Mag_50")

## NON-PEAK, NON-MAG:
#metrics_nonpeakmag <- c("FA_Tim","FA_Dur","Wet_Tim","Wet_BFL_Dur","SP_Tim","SP_Dur","SP_ROC","DS_Tim","DS_Dur_WS")

## PEAK metrics:
#metrics_peak <- c("Peak_2","Peak_Dur_2","Peak_Fre_2","Peak_5","Peak_Dur_5","Peak_Fre_5","Peak_10","Peak_Dur_10","Peak_Fre_10")

# STEP 0: Setup Paths and Folders -----------------------------------

## path to the NHD predictor data (csv with accum vals for each COMID)
nhd_predictor_input <- "data_input/model_application/CA_NHDPreds/"

## list all csv file names with NHD predictors
comlist<-list.files(nhd_predictor_input, pattern = "*csv")

## Specify a metric(s)
#curmet <- "Peak_2" # ribbit

curmets <- c("FA_Tim") # ribbit ribbit

# STEP 1: Run RF model for metric -----------------------------------

#rf <- f_run_rf_model(curmet, met) # single

rfs <- map(curmets, ~f_run_rf_model(.x, met))

# STEP 2: Make FFM Predictions from RF model -----------------------------------

# map(comlist, ~f_make_ffm_preds(rf = rf, csv = .x)) # single metric

for(i in seq_along(curmets)){
  print(glue("Working on {curmets[i]}"))
  # 1. Make preds ------------------------
  map(comlist, ~f_make_ffm_preds(rf = rfs[[i]], csv = .x ))
  # 2. List csv of individual preds ------
  listcsv <- list.files(path = paste0("model_output/modresults"),
                        pattern = "*.csv")
  # 3. Condense into one file ------------
  nhd <- read_csv(glue("model_output/modresults/{listcsv}"))
  # 4. Export files ----------------------
  f_write_ffm_out(nhd_metrics = nhd, metric = curmets[i])
  # 5. Remove temp files -----------------
  fs::file_delete(fs::dir_ls("model_output/modresults"))

}



