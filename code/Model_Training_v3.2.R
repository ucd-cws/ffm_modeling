#### Modeling Training Using LOOCV Approach With Variable Importance Assessment
#### Version 3.2, updated August 31, 2021 by Ted Grantham (tgrantham@berkeley.edu)

options(max.print=9999999)
filepath <- ("~/Documents/ACTIVE/CEFF FFM modeling/data/model_training/")

## Load data with all FFM observations and associated watershed variables
met <- read.csv(paste(filepath,"met.csv",sep=""))

## Begin modeling
library(randomForest)
library(dplyr)
library(data.table)
library(foreach)
library(doParallel)
registerDoParallel(cores=4)

#########################################################################################################################
#### Loop through list of magnitude metrics (excluding peak flow magnitude metrics), build models and output results ####
#########################################################################################################################
ffms <- c("FA_Mag","Wet_BFL_Mag_50","Wet_BFL_Mag_10","SP_Mag","DS_Mag_90","DS_Mag_50")
dir.create(paste(filepath,"temp_results",sep=""))

for(curmet in ffms){
  tmet<-subset(met,Stat==curmet)
  setwd(paste(filepath,"temp_results",sep=""))
  slist<-as.character(unique(tmet$TID))
  foreach(idx=1:length(slist))%dopar%{
    clist<-slist[-idx]
    tmet.c<-tmet[tmet$TID %in% clist,]
    tmet.v<-tmet[tmet$TID %in% slist[idx],]
    rf<-randomForest(Value/DRAIN_SQKM~.,tmet.c[,c(5,9:116,124:215)],ntree=2000)
    # Note that magnitude metrics are scaled by drainage area
    
  ### Save variable importance
    varimp<-as.data.frame(rf$importance)
    fwrite(varimp,paste(idx,"varimp.csv",sep=""),row.names=TRUE)
  
  ### Predict to validation sites and save median, 10th, 25th, 75th, & 90th percentiles
    preds<-predict(rf,tmet.v,predict.all=TRUE)
    predp50<-apply(preds$individual,1,median)
    predp10<-apply(preds$individual,1,function(x) quantile(x,probs=0.1))
    predp25<-apply(preds$individual,1,function(x) quantile(x,probs=0.25))
    predp75<-apply(preds$individual,1,function(x) quantile(x,probs=0.75))
    predp90<-apply(preds$individual,1,function(x) quantile(x,probs=0.9))
    val<-data.frame(TID=tmet.v$TID,WY=tmet.v$Year,Obs=tmet.v$Value,AREA=tmet.v$DRAIN_SQKM,
                    p50=predp50,p10=predp10,p25=predp25,p75=predp75,p90=predp90)
    fwrite(val,paste(slist[idx],"_valpreds.csv",sep=""),row.names=FALSE)
    rm(preds)
  }
  
  ### Read in and aggregate predictions from LOOCV
  val<-NULL
  for(cursite in slist){
    dodo<-read.csv(paste(cursite,"_valpreds.csv",sep=""),as.is=TRUE)
    val<-rbind(val,dodo)
  }
  val$FFM<-curmet
  write.csv(val,paste(filepath,"model_results/Perform/",curmet,"_Perform.csv",sep=""),row.names=FALSE)
  
  ### Read in and aggregate predictor variable importance
  varimp<-NULL
  for(i in 1:length(slist)){
    dodo<-read.csv(paste(i,"varimp.csv",sep=""),as.is=TRUE)
    names(dodo)[1]<-"Predictor"
    dodo$Model.No<-i
    varimp<-rbind(dodo,varimp)
  }
  write.csv(varimp,paste(filepath,"model_results/VarImp/",curmet,"_VarImp.csv",sep=""),row.names=FALSE)
  
  ### Delete files in modresults director
  setwd(paste(filepath,"temp_results/",sep=""))
  file.remove(list.files())
}

### Clean files
file.remove(paste(filepath,"temp_results/",sep=""))
rm(dodo,tmet,val,varimp,curmet,cursite,ffms,i,slist)

#########################################################################################################################
#### Loop through list of peak flow magnitude metrics, build models and output results ####
#########################################################################################################################
ffms<-c("Peak_2", "Peak_5", "Peak_10")
dir.create(paste(filepath,"temp_results",sep=""))

for(curmet in ffms){
  tmet<-subset(met,Stat==curmet)
  setwd(paste(filepath,"temp_results",sep=""))
  slist<-as.character(unique(tmet$TID))
  foreach(idx=1:length(slist))%dopar%{
    clist<-slist[-idx]
    tmet.c<-tmet[tmet$TID %in% clist,]
    tmet.v<-tmet[tmet$TID %in% slist[idx],]
    rf<-randomForest(Value/DRAIN_SQKM~.,tmet.c[,c(5,110:116,124:215)],ntree=2000) 
    # Note smaller subset of variables used in model training because peak flow mag metrics do not vary by water year
    # Note that magnitude metrics are scaled by drainage area
    
  ### Save variable importance
    varimp<-as.data.frame(rf$importance)
    fwrite(varimp,paste(idx,"varimp.csv",sep=""),row.names=TRUE)
  
  ### Predict to validation sites and save median, 10th, 25th, 75th, & 90th percentiles
    preds<-predict(rf,tmet.v,predict.all=TRUE)
    predp50<-apply(preds$individual,1,median)
    predp10<-apply(preds$individual,1,function(x) quantile(x,probs=0.1))
    predp25<-apply(preds$individual,1,function(x) quantile(x,probs=0.25))
    predp75<-apply(preds$individual,1,function(x) quantile(x,probs=0.75))
    predp90<-apply(preds$individual,1,function(x) quantile(x,probs=0.9))
    val<-data.frame(TID=tmet.v$TID,WY=tmet.v$Year,Obs=tmet.v$Value,AREA=tmet.v$DRAIN_SQKM,
                    p50=predp50,p10=predp10,p25=predp25,p75=predp75,p90=predp90)
    fwrite(val,paste(slist[idx],"_valpreds.csv",sep=""),row.names=FALSE)
    rm(preds)
  }
  
  ### Read in and aggregate predictions from LOOCV
  val<-NULL
  for(cursite in slist){
    dodo<-read.csv(paste(cursite,"_valpreds.csv",sep=""),as.is=TRUE)
    val<-rbind(val,dodo)
  }
  val$FFM<-curmet
  write.csv(val,paste(filepath,"model_results/Perform/",curmet,"_Perform.csv",sep=""),row.names=FALSE)
  
  ### Read in and aggregate predictor variable importance
  varimp<-NULL
  for(i in 1:length(slist)){
    dodo<-read.csv(paste(i,"varimp.csv",sep=""),as.is=TRUE)
    names(dodo)[1]<-"Predictor"
    dodo$Model.No<-i
    varimp<-rbind(dodo,varimp)
  }
  write.csv(varimp,paste(filepath,"model_results/VarImp/",curmet,"_VarImp.csv",sep=""),row.names=FALSE)
  
  ### Delete files in modresults director
  setwd(paste(filepath,"temp_results/",sep=""))
  file.remove(list.files())
}

### Clean files
file.remove(paste(filepath,"temp_results/",sep=""))
rm(dodo,tmet,val,varimp,curmet,cursite,ffms,i,slist)


#########################################################################################################################
#### Loop through remaining non-magnitude metrics ####
#########################################################################################################################
ffms <- c("FA_Tim","FA_Dur","Wet_Tim","Wet_BFL_Dur","Peak_Dur_2","Peak_Fre_2","Peak_Dur_5","Peak_Fre_5",
          "Peak_Dur_10","Peak_Fre_10","SP_Tim","SP_Dur","SP_ROC","DS_Tim","DS_Dur_WS")
dir.create(paste(filepath,"temp_results",sep=""))

for(curmet in ffms){
  tmet<-subset(met,Stat==curmet)
  setwd(paste(filepath,"temp_results",sep=""))
  slist<-as.character(unique(tmet$TID))
  foreach(idx=1:length(slist))%dopar%{
    clist<-slist[-idx]
    tmet.c<-tmet[tmet$TID %in% clist,]
    tmet.v<-tmet[tmet$TID %in% slist[idx],]
    rf<-randomForest(Value~.,tmet.c[,c(5,9:116,124:215)],ntree=2000)
    # Note that these metrics are not scaled by drainage area
    
    ### Save variable importance
    varimp<-as.data.frame(rf$importance)
    fwrite(varimp,paste(idx,"varimp.csv",sep=""),row.names=TRUE)
    
    ### Predict to validation sites and save median, 10th, 25th, 75th, & 90th percentiles
    preds<-predict(rf,tmet.v,predict.all=TRUE)
    predp50<-apply(preds$individual,1,median)
    predp10<-apply(preds$individual,1,function(x) quantile(x,probs=0.1))
    predp25<-apply(preds$individual,1,function(x) quantile(x,probs=0.25))
    predp75<-apply(preds$individual,1,function(x) quantile(x,probs=0.75))
    predp90<-apply(preds$individual,1,function(x) quantile(x,probs=0.9))
    val<-data.frame(TID=tmet.v$TID,WY=tmet.v$Year,Obs=tmet.v$Value,AREA=tmet.v$DRAIN_SQKM,
                    p50=predp50,p10=predp10,p25=predp25,p75=predp75,p90=predp90)
    fwrite(val,paste(slist[idx],"_valpreds.csv",sep=""),row.names=FALSE)
    rm(preds)
  }
  
  ### Read in and aggregate predictions from LOOCV
  val<-NULL
  for(cursite in slist){
    dodo<-read.csv(paste(cursite,"_valpreds.csv",sep=""),as.is=TRUE)
    val<-rbind(val,dodo)
  }
  val$FFM<-curmet
  write.csv(val,paste(filepath,"model_results/Perform/",curmet,"_Perform.csv",sep=""),row.names=FALSE)
  
  ### Read in and aggregate predictor variable importance
  varimp<-NULL
  for(i in 1:length(slist)){
    dodo<-read.csv(paste(i,"varimp.csv",sep=""),as.is=TRUE)
    names(dodo)[1]<-"Predictor"
    dodo$Model.No<-i
    varimp<-rbind(dodo,varimp)
  }
  write.csv(varimp,paste(filepath,"model_results/VarImp/",curmet,"_VarImp.csv",sep=""),row.names=FALSE)
  
  ### Delete files in modresults director
  setwd(paste(filepath,"temp_results/",sep=""))
  file.remove(list.files())
}

### Clean files
file.remove(paste(filepath,"temp_results/",sep=""))
rm(dodo,tmet,val,varimp,curmet,cursite,ffms,i,slist)

#########################################################################################################################
#### Compile and summarize variable importance results for each FFM ####
#########################################################################################################################
ffms <- c("FA_Mag","FA_Tim","FA_Dur","Wet_BFL_Mag_50","Wet_BFL_Mag_10","Wet_Tim","Wet_BFL_Dur","Peak_2",
          "Peak_Dur_2","Peak_Fre_2","Peak_5","Peak_Dur_5","Peak_Fre_5","Peak_10","Peak_Dur_10","Peak_Fre_10",
          "SP_Mag","SP_Tim","SP_Dur","SP_ROC","DS_Mag_90","DS_Mag_50","DS_Tim","DS_Dur_WS")

setwd(paste(filepath,"model_results/VarImp",sep=""))


### Calculate mean value of Gini impurity index for each variable across all model iterations
varimp_summary <- data.frame()
varimp_t10_summary <- data.frame()

for(i in 1:length(ffms)){
  curmet = ffms[i]
  varimp <- read.csv(paste(curmet,"_VarImp.csv",sep=""))
  varimp <- group_by(varimp,Predictor)
  sum_temp <- summarize(varimp, avg = mean(IncNodePurity)) 
  sum_temp$FFM <- curmet
  sum_temp2 <- top_n(sum_temp,10,avg)
  varimp_summary <- rbind(varimp_summary, sum_temp)
  varimp_t10_summary <- rbind(varimp_t10_summary, sum_temp2)
}

write.csv(varimp_summary, "All_FFMs_VarImp.csv",row.names = F)
write.csv(varimp_t10_summary, "All_FFMs_VarImp_top10.csv",row.names = F)




