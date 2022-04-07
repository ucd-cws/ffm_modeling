
# run predictions using random forest model
# based on T. Grantham's CEFF flow modeling
# just packaged that code into a function for more reproducibility


# Libraries ---------------------------------------------------------------

library(fs)
library(glue)
library(data.table)

## Function to Scale Predictions by Area if Magnitude Metric -----------------

f_write_ffm_out <- function(nhd_metrics, metric){

  # Here is the list of metrics to predict across the stream network:
  ## MAG/Peak metrics:
  metrics_mag <- c("FA_Mag","Wet_BFL_Mag_50","Wet_BFL_Mag_10","SP_Mag","DS_Mag_90","DS_Mag_50", "Peak_2","Peak_5","Peak_10")

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
