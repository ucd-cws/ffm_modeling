# run random forest model
# based on T. Grantham's CEFF flow modeling
# just packaged that code into a function for more reproducibility


# Metrics -----------------------------------------------------------------

# Here is the list of metrics to predict across the stream network:
## MAG metrics:
metrics_mag <- c("FA_Mag","Wet_BFL_Mag_50","Wet_BFL_Mag_10","SP_Mag","DS_Mag_90","DS_Mag_50")

## NON-PEAK, NON-MAG:
metrics_nonpeakmag <- c("FA_Tim","FA_Dur","Wet_Tim","Wet_BFL_Dur","SP_Tim","SP_Dur","SP_ROC","DS_Tim","DS_Dur_WS")

## PEAK metrics:
metrics_peak <- c("Peak_2","Peak_Dur_2","Peak_Fre_2","Peak_5","Peak_Dur_5","Peak_Fre_5","Peak_10","Peak_Dur_10","Peak_Fre_10")

# Libraries ---------------------------------------------------------------

library(glue) # better pasting of things together
library(randomForest)

# Random Forest Model -----------------------------------------------------

# run random forest model for each metric:
# assumes data with metrics and column names match

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

