# Split by COMID for input to model_application

library(readr)
library(dplyr)
library(purrr)
library(glue)
library(janitor)

# read in accum data
source("code/f_import_accumulation_data.R")
dat_input <- f_import_accum_data()

# read in xwalk
xwalk <- read_csv("data_input/model_application/input_var_names_xwalk.csv")

# so these cols are missing from accumulation (new input data),
# but not really relevant in model:
# c("pmpe", "bdmax", "pmax_ws", "tmin_ws", "permh_ws", "pmin_ws"

# set names to match for modeling
dat_input <- dat_input %>%
  set_names(xwalk$examp)

#View(as.data.frame(names(dat_input))) # check

# split out
dat_split <- dat_input %>% group_split(comid) %>%
  set_names(., unique(dat_input$comid))
names(dat_split) # check each is named


# write out
map2(dat_split, names(dat_split), ~write_csv(.x, file = glue("data_input/model_application/CA_NHDPreds/{.y}.csv")))
