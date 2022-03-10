# Generate/Import Data for Model Application
# This is based on the accumulation process for given watershed
# using code via Ryan Peek (https://github.com/ryanpeek/ffm_accumulation/)

library(readr)
library(dplyr)
library(contentid)
library(glue)

f_import_accum_data <- function() {

  # get raw data ID:
  (data <- contentid::store(glue("https://github.com/ryanpeek/ffm_accumulation/blob/main/data_clean/08_accumulated_all_metrics.csv?raw=true")))

  # this is hardcoded so if accumulation file changes, we know and re-import
  data_file <- contentid::resolve("hash://sha256/443f94b233e09faea7a7a71b4c83ce1472081f321baec7e13822c6d3897dfdd9")

  # read in data
  accum_dat <- read_csv(data_file)

  print("Data loading complete.")

  return(accum_dat)

}

# use as follows:
# dat_input <- f_import_accum_data()
