#### Apply FFM Models to CA NHD Network

#### Version 3.3 updated Mar 10, 2022 by Ryan Peek (rapeek@ucdavis.edu)
#### Version 3.2, updated September 21, 2021 by Ted Grantham (tgrantham@berkeley.edu)

# GETTING ACCUMULATION REVISED DATA INTO R (read from github: https://github.com/ryanpeek/ffm_accumulation/blob/main/data_clean/08_accumulated_all_metrics.csv)
## run: f_import_accumulation_data
# Format and make sure matches model application input needs
### clean and check? run: check_accumulation_colnames.R
## Split out by comid, unique csv for each comid
### split_by_comid_export.R


# Libraries ---------------------------------------------------------------
library(dplyr)
library(readr) # read zipped csv
library(fs) # more stable file paths
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


## Manually Select FFM
# Here is the list of metrics that we'd like to predict across the stream network:
# "FA_Mag","FA_Tim","FA_Dur","Wet_BFL_Mag_50","Wet_BFL_Mag_10","Wet_Tim","Wet_BFL_Dur","Peak_2",
# "Peak_Dur_2","Peak_Fre_2","Peak_5","Peak_Dur_5","Peak_Fre_5","Peak_10","Peak_Dur_10","Peak_Fre_10",
#  "SP_Mag","SP_Tim","SP_Dur","SP_ROC","DS_Mag_90","DS_Mag_50","DS_Tim","DS_Dur_WS")

curmet<-"FA_Mag"
tmet<-subset(met,stat==curmet)

# RANDOM FOREST MODEL -----------------------------------------------------

## Create random forest model - select one line of the following code!!!

# Use this code for non-peak magnitude metrics, scaled by drainage area
rf<-randomForest(value/drain_sqkm~.,tmet[,c(5,9:116,124:209)],ntree=2000)

# Use this code for peak flow magnitude metrics, scaled by drainage area
rf<-randomForest(value/drain_sqkm~.,tmet[,c(5,110:116,124:209)],ntree=2000)

# Use this code for remaining metrics (non magnitude), not scaled by drainage area
rf<-randomForest(value~.,tmet[,c(5,9:116,124:209)],ntree=2000)

## Loop through NHD segments, apply to RF model, then save out
filepath <- ("data_input/model_application/")
#setwd(paste(filepath,"modresults",sep="")) ## NOTE this is a temporary directory for storing output across the parallel processing

comlist<-list.files(fs::path(filepath, "CA_NHDPreds/")) ## NOTE this is the directory with the NHD predictor files

foreach(idx=1:length(comlist))%dopar%{
  dodo<-read.csv(fs::path(filepath, "CA_NHDPreds/", comlist[idx]),as.is=TRUE)
  # predict RF to NHD site and save median, 10th, 25th, 75th, & 90th percentiles
  preds<-predict(rf,dodo,predict.all=TRUE)
  predp50<-apply(preds$individual,1,median)
  predp10<-apply(preds$individual,1,function(x) quantile(x,probs=0.1))
  predp25<-apply(preds$individual,1,function(x) quantile(x,probs=0.25))
  predp75<-apply(preds$individual,1,function(x) quantile(x,probs=0.75))
  predp90<-apply(preds$individual,1,function(x) quantile(x,probs=0.9))
  nhd<-data.frame(comid=dodo$comid,wy=dodo$wa_yr,area=dodo$drain_sqkm,p50=predp50,p10=predp10,p25=predp25,p75=predp75,p90=predp90)
  ## write to temporary directory for storing output across the parallel processing
  fwrite(nhd,file = fs::path(filepath, "modresults", comlist[idx]),row.names=FALSE)
}

## Read in all COMIDs and combine into single LIST, then combine into a single, large file
ldf<-list()
listcsv<- dir(path = paste0(filepath, "/modresults"), pattern = "*.csv")
for(k in 1:length(listcsv)){
  ldf[[k]]<-read.csv(paste0(filepath, "/modresults/", listcsv[k]))
}
#
nhd<-rbindlist(ldf)

# create dir
fs::dir_create(paste0(filepath, "/CA_NHD_FFMs"))
fwrite(nhd, paste0(filepath,"/CA_NHD_FFMs/", curmet, "_nhd.csv"))

## For magnitude metrics only, run this code to compile drainage-area corrected predictions
nhd$p10 <- nhd$area * nhd$p10 # scale predicitions to cfs
nhd$p25 <- nhd$area * nhd$p25 # scale predicitions to cfs
nhd$p50 <- nhd$area * nhd$p50 # scale predicitions to cfs
nhd$p75 <- nhd$area * nhd$p75 # scale predicitions to cfs
nhd$p90 <- nhd$area * nhd$p90 # scale predicitions to cfs

fwrite(nhd,paste0(filepath,"/CA_NHD_FFMs/",curmet,"_nhd.csv"))

## Delete files in modresults directory
fs::file_delete(fs::dir_ls(paste0(filepath,"modresults"), glob = "*.csv"))


