#### Modeling Performance Evaluation 
#### Version 3.2, updated December 14, 2021 by Ted Grantham (tgrantham@berkeley.edu)

library(dplyr)

## Read reference gage file with class assignments
class <- read.csv("~/Documents/ACTIVE/CEFF FFM modeling/data/model_performance/FFM_perf_stats_by_class/Final_Reference_Gages_9.20.2018_3class.csv")
class$STAIDT <- paste("T",class$USGS_GAGE, sep="")
ref_gages <- read.csv("~/Documents/ACTIVE/CEFF FFM modeling/data/model_training/FFM_model_ref_gages.csv")
ref_gage_class <- left_join(ref_gages, class, by = "STAIDT")
ref_gage_class <- ref_gage_class[c(1,40)] # model reference gages assigned to one of three classes
ref_gage_class <- na.omit(ref_gage_class) # remove gages not assigned to a class

#### Compile performance stats by class for each FFM ####
filepath <- ("~/Documents/ACTIVE/CEFF FFM modeling/data/model_performance/FFM_perf_stats/")

# first loop through range-based performance metrics, excluding peak flow mag FFMs
ffms <- c("FA_Mag","Wet_BFL_Mag_50","Wet_BFL_Mag_10","SP_Mag","DS_Mag_90","DS_Mag_50",
          "Peak_Dur_2","Peak_Fre_2","Peak_Dur_5","Peak_Fre_5","Peak_Dur_10",
          "Peak_Fre_10","FA_Tim","FA_Dur","Wet_Tim","Wet_BFL_Dur","SP_Tim","SP_Dur","SP_ROC",
          "DS_Tim","DS_Dur_WS")

range_master <- data.frame()
for(i in 1:length(ffms)){
  FFM <- ffms[i]
  range_perf <- read.csv(paste(filepath,ffms[i],"_site_perf.csv", sep=""))
  range_perf <- left_join(range_perf, ref_gage_class, by = "STAIDT")
  range_perf <- na.omit(range_perf)
  for(j in 1:3){
    CLASS <- j
    sub_range <- filter(range_perf, X3_CLASS == j)  
    n_sites <- length(sub_range$STAIDT)
    mean_pIQR <- mean(sub_range$percent_IQR) # site average of percent obs within IQR
    mean_pI80R <- mean(sub_range$percent_I80R) # site average of percent obs within inner 80th percentile range
    sd_pIQR <- sd(sub_range$percent_IQR) # SD of percent obs within IQR
    sd_pI80R <- sd(sub_range$percent_I80R) # SD of percent obs within inner 80th percentile range
    
    # scale values for composite metric calculation
    IQR_scl <- ifelse(mean_pIQR>50,1,mean_pIQR/50)
    I80R_scl <-  ifelse(mean_pI80R>80,1,mean_pI80R/80)
    IQR_scl <- ifelse(mean_pIQR<=50, mean_pIQR/50, (100 - mean_pIQR)/50)
    I80R_scl <-  ifelse(mean_pI80R<=80, mean_pI80R/80, (160 - mean_pI80R)/80)
    
    temp <- data.frame(FFM, CLASS, n_sites, mean_pIQR,sd_pIQR,mean_pI80R,sd_pI80R,IQR_scl,I80R_scl)
    range_master <- rbind(range_master, temp)
  }
}

# next loop through OE-based performance metrics, excluding peak flow timing and duration FFMs
ffms <- c("FA_Mag","Wet_BFL_Mag_50","Wet_BFL_Mag_10","SP_Mag","DS_Mag_90","DS_Mag_50","Peak_2", 
          "Peak_5", "Peak_10","FA_Tim","FA_Dur","Wet_Tim","Wet_BFL_Dur","SP_Tim","SP_Dur","SP_ROC",
          "DS_Tim","DS_Dur_WS")

OE_master <- data.frame()
for(i in 1:length(ffms)){
  FFM <- ffms[i]
  OE_perf <- read.csv(paste(filepath,ffms[i],"_site_OE_perf.csv", sep=""))
  OE_perf <- left_join(OE_perf, ref_gage_class, by = "STAIDT")
  OE_perf <- na.omit(OE_perf)
  for(j in 1:3){
    CLASS <- j
    sub_OE <- filter(OE_perf, X3_CLASS == j)

    # calculate global model performance stats for gages within class
    MnOE <- mean(sub_OE$medOE)
    rsquared <- cor(sub_OE$preds_median, sub_OE$obs_median)^2
    pbias <- ((sum(sub_OE$obs_median - sub_OE$preds_median))*100)/sum(sub_OE$obs_median)
    NSE <-  1 - sum((sub_OE$obs_median - sub_OE$preds_median)^2) / sum((sub_OE$obs_median - mean(sub_OE$obs_median))^2)
      
    # scale values for composite performance metric calculation
    MnOE_scl <- ifelse(MnOE<1,MnOE,1/MnOE)
    pbias_scl <- (100 - abs(pbias))/100
    
    temp <- data.frame(FFM, CLASS, MnOE, rsquared, pbias, NSE, MnOE_scl, pbias_scl)
    OE_master <- rbind(OE_master, temp)
  }
}

## compile master performance tables
master <- full_join(range_master, OE_master, by = c("FFM","CLASS"))

df <- data.frame()
for(i in 1:nrow(master)){
  FFM <- master[i,1]
  data <- master[i,c(8,9,11,13:15)]
  data <- data[, colSums(is.na(data)) == 0]
  comp <- ifelse(ncol(data)==0, "NA", mean(as.numeric(data[,c(2:ncol(data))])))
  temp <- data.frame(FFM,comp)
  df <- rbind(df, temp)
}
master <- cbind(master,df[-1])

## export data
filename <- ("~/Documents/ACTIVE/CEFF FFM modeling/data/model_performance/FFM_performance_master_by_class.csv")
write.csv(master, filename, row.names = F)

