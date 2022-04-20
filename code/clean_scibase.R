# clean scibase and filter to comids of interest

source("R/functions/f_functions.R")
source("R/functions/f_extract_filter_to_comids_single.R")

# get coms
comids <- 7964867
#comids <- read_rds("data_clean/comid_list.rds")
subdir <- "data_raw/scibase_flow/"
outdir <- "data_clean/scibase_flow_tst"

# tst
file_list <- get_zip_list(glue("{subdir}"), "*zip", recurse = FALSE)

# extract
#f_extract_filter_to_comids(subdir = data_dir, comids = coms, outdir = "data_clean/flow_nhd_v2", recurse = FALSE)

alldat <- map(file_list$path,
              ~f_extract_filter_to_comids(.x, comids = comids,
                                          outdir = outdir))

# check dimensions (one way to filter out zeros)
map(alldat, ~nrow(.x)>0) # all items have >0 rows if TRUE

# Drop Zero Data --------------------------------------------------------------

# drop data with zero rows
alldat <- discard(alldat, ~nrow(.x)==0)

# Save into one file ------------------------------------------------------

alldat_combine <- alldat %>%
  reduce(left_join, by = c("COMID")) %>% # join by COMID
  # drop dup columns
  select(-c(ends_with(".x"), contains("NODATA"), starts_with("source")))

# write out
write_csv(alldat_combine, file = glue("{outdir}/scibase_data_merged_by_comids.csv"))



