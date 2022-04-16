# Generate/Import Data for Model Application
# This is based on the accumulation process for given watershed
# using code via Ryan Peek (https://github.com/ryanpeek/ffm_accumulation/)

library(readr)
library(dplyr)
library(contentid)
library(glue)

f_import_accum_data <- function() {

  # get raw data ID:
  (data <- contentid::store(glue("output/lsh_catch_accumulated_metrics.csv")))

  # this is hardcoded so if accumulation file changes, we know and re-import
  data_file <- contentid::resolve("hash://sha256/b7ba17253be1a826b2b6bfd4cdc86f55281cb11f39868301ceb081efcd11af21")

  # read in data
  accum_dat <- read_csv(data_file)

  print("Data loading complete.")

  return(accum_dat)

}

# use as follows:
# dat_input <- f_import_accum_data()
