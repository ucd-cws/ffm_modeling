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
library(data.table)
library(janitor)
library(foreach)
library(doParallel)
library(randomForest)
registerDoParallel(cores=4)

# Load Data ---------------------------------------------------------------

## Load data with all FFM observations and associated watershed variables
met <- read_csv("data_input/model_training/met.csv.zip") %>% clean_names() %>%
  # drop these vars
  select(-c("pmpe", "bdmax", "pmax_ws", "tmin_ws", "permh_ws", "pmin_ws"))

# for interactively seeing col names
#names(met) %>% as.data.frame() %>% View(title = "colnames")

# Metrics -----------------------------------------------------------------

# Here is the list of metrics to predict across the stream network:
## MAG metrics:
metrics_mag <- c("FA_Mag","Wet_BFL_Mag_50","Wet_BFL_Mag_10","SP_Mag","DS_Mag_90","DS_Mag_50")

## NON-PEAK, NON-MAG:
metrics_nonpeakmag <- c("FA_Tim","FA_Dur","Wet_Tim","Wet_BFL_Dur","SP_Tim","SP_Dur","SP_ROC","DS_Tim","DS_Dur_WS")

## PEAK metrics:
metrics_peak <- c("Peak_2","Peak_Dur_2","Peak_Fre_2","Peak_5","Peak_Dur_5","Peak_Fre_5","Peak_10","Peak_Dur_10","Peak_Fre_10")

# can eventually use these lists to loop through and apply model

# FUNCTIONS ---------------------------------------------------------------

# these are custom functions to apply FFM model, written by RPeek
# each can be used in a loop or with the purrr::map functions

## Function RANDOM FOREST MODEL ----------------------------------------

# run random forest model for each metric
f_run_rf_model <- function(metric, data){

  # subset the data
  tmet <- data[data$stat==metric,]
  # now pick correct model dataset based on metric
  if(metric %in% metrics_mag){
    print("non-peak magnitude metric identified...running model")
    # scaled by drainage area for non-peak mag metrics
    rf<-randomForest(value/drain_sqkm~.,tmet[,c(5,9:116,124:209)],ntree=2000)
    print(glue("\nRF Model for {metric} done!"))
  } else if(metric %in% metrics_peak){
    print("Peak flow metric identified...running model")
    # scaled by drainage area for peak metrics
    rf<-randomForest(value/drain_sqkm~.,tmet[,c(5,110:116,124:209)],ntree=2000)
    print(glue("\nRF Model for {metric} done!"))
  } else if(metric %in% metrics_nonpeakmag){
    print("non-peak, non-mag flow metric identified...running model")
    # non peak non-mag metrics not scaled by drainage area
    rf<-randomForest(value~.,tmet[,c(5,9:116,124:209)],ntree=2000)
    print(glue("\nRF Model for {metric} done!"))
  } else(
    print(glue("\nIncorrect metric specified! \ncheck '{metric}' spelling?"))
  )
  return(rf)
}

## Function to Make FFM Predictions ----------------------------------------

# requires rf model, and csv to make predictions from
f_make_ffm_preds <- function(rf, csv){
  # read in csv as is
  dodo <- read.csv(fs::path(nhd_predictor_input, csv), as.is=TRUE)
  # predict RF to NHD site
  preds <- predict(rf, dodo, predict.all=TRUE)
  # save median, 10th, 25th, 75th, & 90th percentiles
  predp50<-apply(preds$individual,1,median)
  predp10<-apply(preds$individual,1,function(x) quantile(x,probs=0.1))
  predp25<-apply(preds$individual,1,function(x) quantile(x,probs=0.25))
  predp75<-apply(preds$individual,1,function(x) quantile(x,probs=0.75))
  predp90<-apply(preds$individual,1,function(x) quantile(x,probs=0.9))
  nhd<-data.frame(comid=dodo$comid,wy=dodo$wa_yr,area=dodo$drain_sqkm,p50=predp50,p10=predp10,p25=predp25,p75=predp75,p90=predp90)
  # write to temp directory to storing output
  fs::dir_create("model_output/modresults")
  fwrite(nhd, file = fs::path("model_output/modresults/", csv),row.names=FALSE)
}

## Function to Scale Predictions by Area if Magnitude Metric -----------------

f_write_ffm_out <- function(nhd_metrics, metric){
  fs::dir_create(paste0("model_output/CA_NHD_FFMs")) # create outdir location
  ## For mag metrics only, compile drainage-area corrected predictions
  if(metric %in% metrics_mag){
    print("magnitude metric identified...scaling and saving out")
    # scale predictions to cfs
    nhd_metrics$p10 <- nhd_metrics$area * nhd_metrics$p10
    nhd_metrics$p25 <- nhd_metrics$area * nhd_metrics$p25
    nhd_metrics$p50 <- nhd_metrics$area * nhd_metrics$p50
    nhd_metrics$p75 <- nhd_metrics$area * nhd_metrics$p75
    nhd_metrics$p90 <- nhd_metrics$area * nhd_metrics$p90
    fwrite(nhd_metrics, paste0("model_output/CA_NHD_FFMs/", metric, "_nhd.csv"))
    print(glue("\n{metric} scaled saved!"))
  } else({
    print(glue("Saving out {metric}..."))
    fwrite(nhd_metrics, paste0("model_output/CA_NHD_FFMs/", metric, "_nhd.csv"))
    print(glue("{metric} saved!"))
  })
  cat("Done")
}

# Apply Model to Get FFM Preds --------------------------------------------

## STEP 0: Setup Paths and Folders

# path to the NHD predictor data (csv with accum vals for each COMID)
nhd_predictor_input <- "data_input/model_application/CA_NHDPreds/"

# list all csv file names with NHD predictors
comlist<-list.files(nhd_predictor_input, pattern = "*csv")

# Specify a metric(s)
curmet <- "Peak_2" # ribbit

## STEP 1: Run RF model for metric
rf <- f_run_rf_model(curmet, met)

## STEP 2: Make FFM Predictions from RF model
map(comlist, ~f_make_ffm_preds(rf = rf, csv = .x))

## STEP 3: Condense Into single File
listcsv<- list.files(path = paste0("model_output/modresults"), pattern = "*.csv")
# Read in all COMIDs and combine into single LIST, combine and save
nhd <- read_csv(glue("model_output/modresults/{listcsv}"))

## STEP 4: Scale/Save Out
f_write_ffm_out(nhd_metrics = nhd, metric = curmet)

## STEP 5: cleanup
## Delete files in modresults directory
fs::file_delete(fs::dir_ls("model_output/modresults"))


