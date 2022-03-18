#### Apply FFM Models to CA NHD Network 
#### Version 3.2, updated September 21, 2021 by Ted Grantham (tgrantham@berkeley.edu)

## Load data with all FFM observations and associated watershed variables
filepath <- ("~/Documents/ACTIVE/CEFF FFM modeling/data/model_training/")
met <- read.csv(paste(filepath,"met.csv",sep=""))

library(data.table)
library(foreach)
library(doParallel)
library(randomForest)
registerDoParallel(cores=4)

## Manually Select FFM
# Here is the list of metrics that we'd like to predict across the stream network: 
# "FA_Mag","FA_Tim","FA_Dur","Wet_BFL_Mag_50","Wet_BFL_Mag_10","Wet_Tim","Wet_BFL_Dur","Peak_2",
# "Peak_Dur_2","Peak_Fre_2","Peak_5","Peak_Dur_5","Peak_Fre_5","Peak_10","Peak_Dur_10","Peak_Fre_10",
#  "SP_Mag","SP_Tim","SP_Dur","SP_ROC","DS_Mag_90","DS_Mag_50","DS_Tim","DS_Dur_WS")

curmet<-"FA_Mag"
tmet<-subset(met,Stat==curmet)

## Create random forest model - select one line of the following code!!! 

# Use this code for non-peak magnitude metrics, scaled by drainage area
rf<-randomForest(Value/DRAIN_SQKM~.,tmet[,c(5,9:116,124:215)],ntree=2000)

# Use this code for peak flow magnitude metrics, scaled by drainage area
rf<-randomForest(Value/DRAIN_SQKM~.,tmet[,c(5,110:116,124:215)],ntree=2000) 

# Use this code for remaining metrics (non magnitude), not scaled by drainage area
rf<-randomForest(Value~.,tmet[,c(5,9:116,124:215)],ntree=2000)

## Loop through NHD segments, apply to RF model, then save out
filepath <- ("~/Documents/ACTIVE/CEFF FFM modeling/data/model_application/")
setwd(paste(filepath,"modresults",sep="")) ## NOTE this is a temporary directory for storing output across the parallel processing

comlist<-list.files(paste(filepath,"CA_NHDPreds/", sep="")) ## NOTE this is the directory with the NHD predictor files

foreach(idx=1:length(comlist))%dopar%{
  dodo<-read.csv(paste(filepath,"/CA_NHDPreds/",comlist[idx],sep=""),as.is=TRUE)
  # predict RF to NHD site and save median, 10th, 25th, 75th, & 90th percentiles
  preds<-predict(rf,dodo,predict.all=TRUE)
  predp50<-apply(preds$individual,1,median)
  predp10<-apply(preds$individual,1,function(x) quantile(x,probs=0.1))
  predp25<-apply(preds$individual,1,function(x) quantile(x,probs=0.25))
  predp75<-apply(preds$individual,1,function(x) quantile(x,probs=0.75))
  predp90<-apply(preds$individual,1,function(x) quantile(x,probs=0.9))
  nhd<-data.frame(COMID=dodo$COMID,WY=dodo$WaYr,AREA=dodo$DRAIN_SQKM,p50=predp50,p10=predp10,p25=predp25,p75=predp75,p90=predp90)
  fwrite(nhd,paste(comlist[idx],sep=""),row.names=FALSE)
}

## Read in all COMIDs and combine into single LIST, then combine into a single, large file
ldf<-list()
listcsv<-dir(pattern=".csv")
for(k in 1:length(listcsv)){
  ldf[[k]]<-read.csv(listcsv[k])
}
#
nhd<-rbindlist(ldf)
fwrite(nhd,paste(filepath,"CA_NHD_FFMs/",curmet,"_nhd.csv",sep=""))

## For magnitude metrics only, run this code to compile drainage-area corrected predictions
nhd$p10 <- nhd$AREA * nhd$p10 # scale predicitions to cfs 
nhd$p25 <- nhd$AREA * nhd$p25 # scale predicitions to cfs 
nhd$p50 <- nhd$AREA * nhd$p50 # scale predicitions to cfs 
nhd$p75 <- nhd$AREA * nhd$p75 # scale predicitions to cfs 
nhd$p90 <- nhd$AREA * nhd$p90 # scale predicitions to cfs 

fwrite(nhd,paste(filepath,"CA_NHD_FFMs/",curmet,"_nhd.csv",sep=""))

## Delete files in modresults directory
file.remove(paste(filepath,"/modresults/", sep=""), list.files(paste(filepath,"/modresults"), sep= ""))


