# Generate/Import Data for Model Application
# This is based on the accumulation process for given watershed
# using code via Ryan Peek (https://github.com/ryanpeek/ffm_accumulation/)

library(readr)
library(dplyr)
library(glue)
library(janitor)

# read in accum data
source("code/f_import_accumulation_data.R")
dat_input <- f_import_accum_data()

# read in original data input to match with
dat_examp <- read_csv("data_input/model_application/example_input_10000042.csv") %>% clean_names() # to match the input

# compare names
compare_df_cols_same(dat_input, dat_examp) # TRUE? Good
compare_df_cols(dat_input, dat_examp, strict_description = TRUE) %>% head(100)

# which colnames don't exist
select(dat_examp, colnames(dat_input))
# `area_sf`, `cat_basin_slope`, `cat_elev_mean`, `cat_elev_min`, `cat_elev_max`
# ? drain_sqkm,  slope_pct_30m, elev_mean_m_basin_30m, elev_min_m_basin_30m, elev_max_m_basin_30m,


# some extra cols, names don't match but same data still
names(dat_input)[207:ncol(dat_input)]
names(dat_examp)[207:ncol(dat_examp)]
colnames(dat_examp)[106:112]
colnames(dat_input)[106:112]

# so these cols are not part of the model
# c("pmpe", "bdmax", "pmax_ws", "tmin_ws", "permh_ws", "pmin_ws"

dat_examp <- dat_examp %>% select(-c("pmpe", "bdmax", "pmax_ws", "tmin_ws", "permh_ws", "pmin_ws"))

# review
names(dat_input)[207:ncol(dat_input)]
names(dat_examp)[207:ncol(dat_examp)]
# matches!

# revise
dat_input_rev <- dat_input
colnames(dat_input_rev) <- colnames(dat_examp)


# make dataframe of colnames just as a double check:
datnames <- tibble("input"= colnames(dat_input),
                   "examp"=colnames(dat_examp)
)

View(datnames)

write_csv(datnames, file = "data_input/model_application/input_var_names_xwalk.csv")
