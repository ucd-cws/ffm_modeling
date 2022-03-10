#### Modeling Performance Evaluation 
#### Version 3.2, updated December 14, 2021 by Ted Grantham (tgrantham@berkeley.edu)

library(dplyr)
library(tidyr)

## Read "FFM_Perform.csv" files from model training to compare observed vs. predicted values
filepath0 <- ("~/Documents/ACTIVE/CEFF FFM modeling/data/model_training/model_results/Perform/")
pred_files <- list.files(filepath0,pattern = "Perform.csv")

#####################################################################################
#### Start with evaluation of all magnitude metrics, excluding peak flow metrics ####
######################################################################################
ffms <- c("FA_Mag","Wet_BFL_Mag_50","Wet_BFL_Mag_10","SP_Mag","DS_Mag_90","DS_Mag_50")

## Loop through metrics and extract performance summary stats
perf_summary <- data.frame() # model performance summary for FFM
perf_OE_summary <- data.frame() # model performance summary of O/E assessment for each FFM

for(i in 1:length(ffms)){
  preds <- read.csv(paste(filepath0,ffms[i],"_Perform.csv",sep=""))
  FFM <- ffms[i] # save FFM ID for labels)
  preds <- rename(preds, STAIDT = TID)
  
  # Scale flow predictions from runoff to cfs by multiplying by drainage area
  preds$Q10 <- preds$AREA * preds$p10 # scale predictions to cfs 
  preds$Q25 <- preds$AREA * preds$p25 # scale predictions to cfs 
  preds$Q50 <- preds$AREA * preds$p50 # scale predictions to cfs 
  preds$Q75 <- preds$AREA * preds$p75 # scale predictions to cfs 
  preds$Q90 <- preds$AREA * preds$p90 # scale predictions to cfs 
  preds <- preds[c(1:3,11:15)] # select relevant vars for analysis
  
  # Screen sites for min 20 years period of record 
  sub <- preds[c(1:2)]
  subg <- group_by(sub, STAIDT)
  site_count <- summarize(subg, n_yrs = n_distinct(WY))
  sites <- filter(site_count, n_yrs >= 20)
  sites <- sites$STAIDT # convert to list
  rm(sub, subg, site_count) # clean up
  
  # Loop through sites to assess model performance
  perf_site <- data.frame()
  perf_OE_site <- data.frame()
  site_stats <- data.frame()
  
  for(j in 1:length(sites)){
    cursite <- sites[j]
    biff <- filter(preds, STAIDT == cursite) # select single site for inspection
    biff <- gather(biff,"data_source","value",3:8)
    biff$data_source <- as.factor(biff$data_source)
    
    # quantiles of observed values
    biffo <- filter(biff, data_source == "Obs")
    biffo <- unique(biffo)
    obs_quants <- as.data.frame(quantile(biffo$value, probs = c(0.10, 0.25, 0.50, 0.75, 0.90)))
    colnames(obs_quants) <- "value"
    
    # quantiles of predicted values (median values of each set of percentile predictions)
    pred_10 <- filter(biff, data_source == "Q10")
    med_10 <- median(pred_10$value); rm(pred_10)
    pred_25 <- filter(biff, data_source == "Q25")
    med_25 <- median(pred_25$value); rm(pred_25)
    pred_50 <- filter(biff, data_source == "Q50")
    med_50 <- median(pred_50$value); rm(pred_50)
    pred_75 <- filter(biff, data_source == "Q75")
    med_75 <- median(pred_75$value); rm(pred_75)
    pred_90 <- filter(biff, data_source == "Q90")
    med_90 <- median(pred_90$value); rm(pred_90)
    
    pred_quants <- c(med_10, med_25, med_50, med_75, med_90)
    quantile <- c("p10", "p25", "p50", "p75", "p90")
    
    # assess observed values in relation to predicted quantiles
    obs_n <- length(biffo$value)
    percent_IQR <- (sum(ifelse(biffo$value <= med_75 & biffo$value >= med_25, 1, 0))/obs_n)*100 # percent obs within IQR
    percent_I80R <- (sum(ifelse(biffo$value <= med_90 & biffo$value >= med_10, 1, 0))/obs_n)*100 # percent obs within inner 80th percentile range
    
    # compile observed vs. predicted quantile values for O/E evaluation and plots
    biffy <- data.frame(STAIDT = cursite, quantile, obs = obs_quants$value, preds = pred_quants)
    rownames(biffy) <- NULL
    biffy.50 <- filter(biffy, quantile == "p50") # save median values
    biffy.50 <- biffy.50[c(1,3,4)]
    biffy.50$medOE <- (biffy.50$obs+1)/(biffy.50$preds+1) # add one to avoid distortion of critiera from low-flow values
    biffy.50$FFM <- FFM
    colnames(biffy.50) <- c("STAIDT", "obs_median", "preds_median", "medOE", "FFM")
    
    # reformat and save for plots
    biffy <- gather(biffy,"data_source","value",3:4) # reformat data
    biffy$FFM <- FFM
    site_stats <- rbind(site_stats, biffy)
    
    # save site performance evaluation data for global performance evaluation
    temp <- data.frame(FFM, STAIDT = cursite, obs_n, percent_IQR, percent_I80R)
    perf_site <- rbind(perf_site, temp)
    perf_OE_site <- rbind(perf_OE_site, biffy.50)
  }
  
  # calculate 'global' model performance stats
  n_sites <- length(sites)
  mean_pIQR <- mean(perf_site$percent_IQR) # site average of percent obs within IQR
  mean_pI80R <- mean(perf_site$percent_I80R) # site average of percent obs within inner 80th percentile range
  sd_pIQR <- sd(perf_site$percent_IQR) # SD of percent obs within IQR
  sd_pI80R <- sd(perf_site$percent_I80R) # SD of percent obs within inner 80th percentile range

  temp <- data.frame(FFM, n_sites, mean_pIQR, sd_pIQR, mean_pI80R, sd_pI80R)
  perf_summary <- rbind(perf_summary, temp)
  
  # calculate 'global' OE stats
  perf_OE_site <- na.omit(perf_OE_site)
  MnOE <- mean(perf_OE_site$medOE)
  rsquared <- cor(perf_OE_site$preds_median, perf_OE_site$obs_median)^2
  pbias <- ((sum(perf_OE_site$obs_median - perf_OE_site$preds_median))*100)/sum(perf_OE_site$obs_median)
  NSE <-  1 - sum((perf_OE_site$obs_median - perf_OE_site$preds_median)^2) / sum((perf_OE_site$obs_median - mean(perf_OE_site$obs_median))^2)
  n_sites <- length(perf_OE_site$obs_median)
  
  temp <- data.frame(FFM, n_sites, MnOE, rsquared, pbias, NSE)
  perf_OE_summary <- rbind(perf_OE_summary, temp)
  
  # export site files
  filepath <- ("~/Documents/ACTIVE/CEFF FFM modeling/data/model_performance/FFM_perf_stats/")
  filename1 <- paste(filepath, FFM, "_site_quantiles.csv", sep="")
  write.csv(site_stats, filename1, row.names = F)
  
  filename2 <- paste(filepath, FFM, "_site_perf.csv", sep="")
  write.csv(perf_site, filename2, row.names = F)
  
  filename3 <- paste(filepath, FFM, "_site_OE_perf.csv", sep="")
  write.csv(perf_OE_site, filename3, row.names = F)
}

filename1 <- paste(paste(filepath, "ALL_MAG_FFM_perf_summary.csv", sep=""))
write.csv(perf_summary, filename1, row.names = F)

filename2 <- paste(paste(filepath,"ALL_MAG_FFM_perf_OE_summary.csv", sep=""))
write.csv(perf_OE_summary, filename2, row.names = F)

# Clean up
rm(biff, biffo, biffy, biffy.50, obs_quants, perf_OE_site, perf_site, perf_summary, perf_OE_summary, preds, site_stats, temp,  
   cursite, FFM, ffms, filepath, filename1, filename2, filename3, i, j, mean_pI80R, mean_pIQR, med_10, med_25, med_50, med_75, 
   med_90,  MnOE, n_sites, NSE, obs_n, pbias, percent_IQR, percent_I80R, pred_quants, quantile, rsquared, sd_pI80R, sd_pIQR, sites)


#####################################################################################
####    Next evaluate model performance for peak flow metrics                    ####
#####################################################################################

## Note: Performance assessed by comparison of observed to predicted values only because peak and high flow... 
## ... metrics are calculated from the long-term record and are not year-specific. Therefore, there is...
## ... not a range of observed values to compare against prediction intervals.

ffms<-c("Peak_2", "Peak_5", "Peak_10")

## Loop through metrics and extract performance summary stats
perf_OE_summary <- data.frame() # model performance summary of O/E assessment for each FFM

for(i in 1:length(ffms)){
  preds <- read.csv(paste(filepath0,ffms[i],"_Perform.csv",sep=""))
  FFM <- ffms[i] # save FFM ID for labels)
  preds <- rename(preds, STAIDT = TID)
  
  # Screen sites for min 20 years period of record 
  sites <- read.csv("/Users/tgrantham/Documents/ACTIVE/CEFF FFM modeling/data/model_training/FFM_model_ref_gages.csv")
  sites <- filter(sites, N_YEAR >= 20)
  preds <- semi_join(preds, sites, by = "STAIDT")
  
  # Scale flow predictions from runoff to cfs by multiplying by drainage area
  preds$Q10 <- preds$AREA * preds$p10 # scale predictions to cfs 
  preds$Q25 <- preds$AREA * preds$p25 # scale predictions to cfs 
  preds$Q50 <- preds$AREA * preds$p50 # scale predictions to cfs 
  preds$Q75 <- preds$AREA * preds$p75 # scale predictions to cfs 
  preds$Q90 <- preds$AREA * preds$p90 # scale predictions to cfs 
  preds <- preds[c(1:2,9:14)] # select relevant vars for analysis
  
  # Export predicted quantile and observed data for plotting
  biff <- gather(preds,"data_source","value",c(2,4:8))
  biff$data_source <- as.factor(biff$data_source)
  
  filepath <- ("~/Documents/ACTIVE/CEFF FFM modeling/data/model_performance/FFM_perf_stats/")
  filename <- paste(filepath, FFM, "_site_quantiles.csv", sep="")
  write.csv(biff, filename, row.names = F)
  
  # Compile observed vs. median predicted (Q50) values
  biffy <- preds[c(1,2,6)]
  biffy <- rename(biffy, obs_median = Obs, preds_median = Q50)
  biffy$medOE <- biffy$obs_median/biffy$preds_median
  biffy$FFM <- FFM
  
  # Export file 
  filename <- paste(filepath, FFM, "_site_OE_perf.csv", sep = "")
  write.csv(biffy, filename, row.names = F)
  
  # Calculate O-E performance metrics
  n_sites <- length(biffy$STAIDT)
  MnOE <- mean(biffy$medOE)
  rsquared <- cor(biffy$preds_median, biffy$obs_median)^2
  pbias <- ((sum(biffy$obs_median - biffy$preds_median))*100)/sum(biffy$obs_median)
  NSE <-  1 - sum((biffy$obs_median - biffy$preds_median)^2) / sum((biffy$obs_median - mean(biffy$obs_median))^2)
  
  temp <- data.frame(FFM, n_sites, MnOE, rsquared, pbias, NSE)
  perf_OE_summary <- rbind(perf_OE_summary, temp)
}

filename <- paste(filepath, "ALL_PEAK_MAG_FFM_perf_OE_summary.csv", sep="")
write.csv(perf_OE_summary, filename, row.names = F)

filename2 <- paste(filepath,"ALL_PEAK_MAG_FFM_perf_summary.csv", sep="") # create quantile perf summary file with NA values
perf_summary <- data.frame(ffms, n_sites); perf_summary <- rename(perf_summary, "FFM" = ffms)
perf_summary[c("mean_pIQR","sd_pIQR","mean_pI80R","sd_pI80R")] <- NA
write.csv(perf_summary, filename2, row.names = F)

# Clean up
rm(biff, sites, biffy, perf_OE_summary, perf_summary,preds, temp, FFM, ffms, filename, filename2,filepath, i, MnOE, n_sites, NSE, pbias, rsquared) 

#####################################################################################
#### Next evaluate all other non-magnitude peak flow metrics                     ####
#####################################################################################

ffms <- c("Peak_Dur_2","Peak_Fre_2","Peak_Dur_5","Peak_Fre_5","Peak_Dur_10","Peak_Fre_10")
filepath <- ("~/Documents/ACTIVE/CEFF FFM modeling/data/model_performance/FFM_perf_stats/")

## Loop through metrics and extract performance summary stats
perf_summary <- data.frame() # model performance summary for FFM

for(i in 1:length(ffms)){
  preds <- read.csv(paste(filepath0,ffms[i],"_Perform.csv",sep=""))
  FFM <- ffms[i] # save FFM ID for labels
  preds <- rename(preds, STAIDT = TID)
  
  # Screen sites for min 20 years period of record used for FFM peak mag estimation
  peak_ffm <- ifelse(i == 1 | i ==2,"Peak_2",ifelse(i == 3 | i ==4,"Peak_5","Peak_10"))
  sites <- read.csv(paste(filepath,peak_ffm,"_site_OE_perf.csv",sep=""))
  preds <- semi_join(preds, sites, by = "STAIDT")
  preds <- filter(preds, Obs>0) # filter out years in which peak flow event did not occur
  
  sub <- preds[c(1:3)]
  subg <- group_by(sub, STAIDT)
  site_count <- summarize(subg, n_yrs = n_distinct(WY))
  sites <- filter(site_count, n_yrs >= 5) # only include sites with 5 or more observations
  sites <- sites$STAIDT # convert to list
  rm(sub, subg, site_count) # clean up

  # Loop through sites to assess model performance
  perf_site <- data.frame()

  for(j in 1:length(sites)){
    cursite <- sites[j]
    biff <- filter(preds, STAIDT == cursite) # select single site for inspection
    biff <- gather(biff,"data_source","value",c(3,5:9))
    biff$data_source <- as.factor(biff$data_source)
    
    # quantiles of observed values
    biffo <- filter(biff, data_source == "Obs")
    biffo <- unique(biffo)

    # quantiles of predicted values (median values of each set of percentile predictions)
    pred_10 <- filter(biff, data_source == "p10")
    med_10 <- median(pred_10$value); rm(pred_10)
    pred_25 <- filter(biff, data_source == "p25")
    med_25 <- median(pred_25$value); rm(pred_25)
    pred_50 <- filter(biff, data_source == "p50")
    med_50 <- median(pred_50$value); rm(pred_50)
    pred_75 <- filter(biff, data_source == "p75")
    med_75 <- median(pred_75$value); rm(pred_75)
    pred_90 <- filter(biff, data_source == "p90")
    med_90 <- median(pred_90$value); rm(pred_90)
    
    pred_quants <- c(med_10, med_25, med_50, med_75, med_90)
    quantile <- c("p10", "p25", "p50", "p75", "p90")
    
    # assess observed values in relation to predicted quantiles
    obs_n <- length(biffo$value)
    percent_IQR <- (sum(ifelse(biffo$value <= med_75 & biffo$value >= med_25, 1, 0))/obs_n)*100 # percent obs within IQR
    percent_I80R <- (sum(ifelse(biffo$value <= med_90 & biffo$value >= med_10, 1, 0))/obs_n)*100 # percent obs within inner 80th percentile range
    
    # save site performance evaluation data for global performance evaluation
    temp <- data.frame(FFM, STAIDT = cursite, obs_n, percent_IQR, percent_I80R)
    perf_site <- rbind(perf_site, temp)
    }
  
  # calculate 'global' model performance stats
  n_sites <- length(sites)
  mean_pIQR <- mean(perf_site$percent_IQR) # site average of percent obs within IQR
  mean_pI80R <- mean(perf_site$percent_I80R) # site average of percent obs within inner 80th percentile range
  sd_pIQR <- sd(perf_site$percent_IQR) # SD of percent obs within IQR
  sd_pI80R <- sd(perf_site$percent_I80R) # SD of percent obs within inner 80th percentile range
  
  temp <- data.frame(FFM, n_sites, mean_pIQR, sd_pIQR, mean_pI80R, sd_pI80R)
  perf_summary <- rbind(perf_summary, temp)
  
  # export site files
  filename1 <- paste(filepath, FFM, "_site_perf.csv", sep="")
  write.csv(perf_site, filename1, row.names = F)
}



filename1 <- paste(paste(filepath, "ALL_OTHER_PEAK_FFM_perf_summary.csv", sep=""))
write.csv(perf_summary, filename1, row.names = F)

filename2 <- paste(paste(filepath,"ALL_OTHER_PEAK_FFM_perf_OE_summary.csv", sep="")) # create OE summary file with NA values
perf_OE_summary <- data.frame(ffms, n_sites); perf_OE_summary <- rename(perf_OE_summary, "FFM" = ffms)
perf_OE_summary[c("MnOE","rsquared","pbias","NSE")] <- NA
write.csv(perf_OE_summary, filename2, row.names = F)

# Clean up
rm(biff, biffo, perf_site, perf_summary, perf_OE_summary, preds, temp, peak_ffm,
   cursite, FFM, ffms, filepath, filename1, filename2, i, j, mean_pI80R, mean_pIQR, med_10, med_25, med_50, med_75, 
   med_90, n_sites, obs_n, percent_IQR, percent_I80R, pred_quants, quantile, sd_pI80R, sd_pIQR, sites)


#####################################################################################
#### Finish with evaluation of all other non-magnitude metrics                   ####
#####################################################################################
ffms <- c("FA_Tim","FA_Dur","Wet_Tim","Wet_BFL_Dur","SP_Tim","SP_Dur","SP_ROC","DS_Tim","DS_Dur_WS")

## Loop through metrics and extract performance summary stats
perf_summary <- data.frame() # model performance summary for FFM
perf_OE_summary <- data.frame() # model performance summary of O/E assessment for each FFM

for(i in 1:length(ffms)){
  preds <- read.csv(paste(filepath0,ffms[i],"_Perform.csv",sep=""))
  FFM <- ffms[i] # save FFM ID for labels)
  preds <- rename(preds, STAIDT = TID)
  
  # Screen sites for min 20 years period of record and remove zero values
  sub <- preds[c(1:3)]
  sub <- filter(sub, Obs > 0)
  subg <- group_by(sub, STAIDT)
  site_count <- summarize(subg, n_yrs = n_distinct(WY))
  sites <- filter(site_count, n_yrs >= 20)
  sites <- sites$STAIDT # convert to list
  rm(sub, subg, site_count) # clean up
  
  # Skip current iteration if no sites can be evaluated
  if(length(sites) == 0) next
  
  # Loop through sites to assess model performance
  perf_site <- data.frame()
  perf_OE_site <- data.frame()
  site_stats <- data.frame()
  
  for(j in 1:length(sites)){
    cursite <- sites[j]
    biff <- filter(preds, STAIDT == cursite) # select single site for inspection
    biff <- gather(biff,"data_source","value",c(3,5:9))
    biff$data_source <- as.factor(biff$data_source)
    
    # quantiles of observed values
    biffo <- filter(biff, data_source == "Obs")
    biffo <- unique(biffo)
    obs_quants <- as.data.frame(quantile(biffo$value, probs = c(0.10, 0.25, 0.50, 0.75, 0.90)))
    colnames(obs_quants) <- "value"
    
    # quantiles of predicted values (median values of each set of percentile predictions)
    pred_10 <- filter(biff, data_source == "p10")
    med_10 <- median(pred_10$value); rm(pred_10)
    pred_25 <- filter(biff, data_source == "p25")
    med_25 <- median(pred_25$value); rm(pred_25)
    pred_50 <- filter(biff, data_source == "p50")
    med_50 <- median(pred_50$value); rm(pred_50)
    pred_75 <- filter(biff, data_source == "p75")
    med_75 <- median(pred_75$value); rm(pred_75)
    pred_90 <- filter(biff, data_source == "p90")
    med_90 <- median(pred_90$value); rm(pred_90)
    
    pred_quants <- c(med_10, med_25, med_50, med_75, med_90)
    quantile <- c("p10", "p25", "p50", "p75", "p90")
    
    # assess observed values in relation to predicted quantiles
    obs_n <- length(biffo$value)
    percent_IQR <- (sum(ifelse(biffo$value <= med_75 & biffo$value >= med_25, 1, 0))/obs_n)*100 # percent obs within IQR
    percent_I80R <- (sum(ifelse(biffo$value <= med_90 & biffo$value >= med_10, 1, 0))/obs_n)*100 # percent obs within inner 80th percentile range
    
    # compile observed vs. predicted quantile values for O/E evaluation and plots
    biffy <- data.frame(STAIDT = cursite, quantile, obs = obs_quants$value, preds = pred_quants)
    rownames(biffy) <- NULL
    biffy.50 <- filter(biffy, quantile == "p50") # save median values
    biffy.50 <- biffy.50[c(1,3,4)]
    biffy.50$medOE <- (biffy.50$obs+1)/(biffy.50$preds+1) # add one to avoid distortion of critiera from low-flow values
    biffy.50$FFM <- FFM
    colnames(biffy.50) <- c("STAIDT", "obs_median", "preds_median", "medOE", "FFM")
    
    # reformat and save for plots
    biffy <- gather(biffy,"data_source","value",3:4) # reformat data
    biffy$FFM <- FFM
    site_stats <- rbind(site_stats, biffy)
    
    # save site performance evaluation data for global performance evaluation
    temp <- data.frame(FFM, STAIDT = cursite, obs_n, percent_IQR, percent_I80R)
    perf_site <- rbind(perf_site, temp)
    perf_OE_site <- rbind(perf_OE_site, biffy.50)
  }
  
  # calculate 'global' model performance stats
  n_sites <- length(sites)
  mean_pIQR <- mean(perf_site$percent_IQR) # site average of percent obs within IQR
  mean_pI80R <- mean(perf_site$percent_I80R) # site average of percent obs within inner 80th percentile range
  sd_pIQR <- sd(perf_site$percent_IQR) # SD of percent obs within IQR
  sd_pI80R <- sd(perf_site$percent_I80R) # SD of percent obs within inner 80th percentile range
  
  temp <- data.frame(FFM, n_sites, mean_pIQR, sd_pIQR, mean_pI80R, sd_pI80R)
  perf_summary <- rbind(perf_summary, temp)
  
  # calculate 'global' OE stats
  perf_OE_site <- na.omit(perf_OE_site)
  MnOE <- mean(perf_OE_site$medOE)
  rsquared <- cor(perf_OE_site$preds_median, perf_OE_site$obs_median)^2
  pbias <- ((sum(perf_OE_site$obs_median - perf_OE_site$preds_median))*100)/sum(perf_OE_site$obs_median)
  NSE <-  1 - sum((perf_OE_site$obs_median - perf_OE_site$preds_median)^2) / sum((perf_OE_site$obs_median - mean(perf_OE_site$obs_median))^2)
  n_sites <- length(perf_OE_site$obs_median)
  
  temp <- data.frame(FFM, n_sites, MnOE, rsquared, pbias, NSE)
  perf_OE_summary <- rbind(perf_OE_summary, temp)
  
  # export site files
  filepath <- ("~/Documents/ACTIVE/CEFF FFM modeling/data/model_performance/FFM_perf_stats/")
  filename1 <- paste(filepath, FFM, "_site_quantiles.csv", sep="")
  write.csv(site_stats, filename1, row.names = F)
  
  filename2 <- paste(filepath, FFM, "_site_perf.csv", sep="")
  write.csv(perf_site, filename2, row.names = F)
  
  filename3 <- paste(filepath, FFM, "_site_OE_perf.csv", sep="")
  write.csv(perf_OE_site, filename3, row.names = F)
}

filename1 <- paste(paste(filepath, "ALL_OTHER_FFM_perf_summary.csv", sep=""))
write.csv(perf_summary, filename1, row.names = F)

filename2 <- paste(paste(filepath,"ALL_OTHER_FFM_perf_OE_summary.csv", sep=""))
write.csv(perf_OE_summary, filename2, row.names = F)

# Clean up
rm(biff, biffo, biffy, biffy.50, obs_quants, perf_OE_site, perf_site, perf_summary, perf_OE_summary, preds, site_stats, temp, pred_files,  
   cursite, FFM, ffms, filepath0, filepath, filename1, filename2, filename3, i, j, mean_pI80R, mean_pIQR, med_10, med_25, med_50, med_75, 
   med_90,  MnOE, n_sites, NSE, obs_n, pbias, percent_IQR, percent_I80R, pred_quants, quantile, rsquared, sd_pI80R, sd_pIQR, sites)


#####################################################################################
#### Compile master performance file                                             ####
#####################################################################################
filepath <- ("~/Documents/ACTIVE/CEFF FFM modeling/data/model_performance/FFM_perf_stats/")

perf_files <- list.files(paste(filepath),pattern = "perf_summary.csv")
OE_files <- list.files(paste(filepath),pattern = "OE_summary.csv")

OE_all <- data.frame()
for(i in 1:length(OE_files)){
  OE <- read.csv(paste(filepath,OE_files[i],sep="")) # Read data from selected metric
  OE_all <- rbind(OE_all, OE)
}

perf_all <- data.frame()
for(i in 1:length(perf_files)){
  perf <- read.csv(paste(filepath,perf_files[i],sep="")) # Read data from selected metric
  perf_all <- rbind(perf_all, perf)
}

master <- left_join(perf_all, OE_all[c(1,3:6)], by = "FFM")

## Calculate composite performance metric
master$IQR_scl <- ifelse(master$mean_pIQR>=50, 1, master$mean_pIQR/50)

master$IQR_scl <- ifelse(master$mean_pIQR<=50, master$mean_pIQR/50, (100 - master$mean_pIQR)/50)
master$I80R_scl <-  ifelse(master$mean_pI80R<=80, master$mean_pI80R/80, (160 - master$mean_pI80R)/80)
master$MnOE_scl <- ifelse(master$MnOE<1,master$MnOE,1/master$MnOE)
master$pbias_scl <- (100 - abs(master$pbias))/100

df <- data.frame()
for(i in 1:nrow(master)){
  data <- master[i,c(1,8,10:14)]
  FFM <- data$FFM
  data <- data[, colSums(is.na(data)) == 0]
  comp <- mean(as.numeric(data[,c(2:ncol(data))]))
  temp <- data.frame(FFM,comp)
  df <- rbind(df, temp)
}
master <- cbind(master,df[-1])

filename <- ("~/Documents/ACTIVE/CEFF FFM modeling/data/model_performance/FFM_performance_master.csv")
write.csv(master, filename, row.names = F)


