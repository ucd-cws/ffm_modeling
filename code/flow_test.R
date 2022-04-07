# flow test

library(tidyverse)
library(tidylog)
library(sf)
library(mapview)
mapviewOptions(fgb = FALSE)
library(glue)
library(here)
library(lubridate)
library(beepr) # to tell us when stuff is done
library(nhdplusTools)

# 01. Load Data ---------------------------------------------------------------

# selected HUC12s
sel_h12 <- read_rds("data_input/catchments_final_lshasta.rds") %>%
  st_transform(4326)

# check comids are integer?
sel_h12 <- sel_h12 %>%
  mutate(COMID = as.integer(comid))

# 02. CHECK COMIDs from NHDTools ------------------------------------------------

# use nhdtools to get comids
# h12_coms <- sel_h12 %>%
#   group_split(FEATUREID) %>%
#   map(~discover_nhdplus_id(.x$geom))

# flatten into single dataframe instead of list
# bmi_segs_df <-bmi_all_coms %>% flatten_dfc() %>% t() %>%
#   as.data.frame() %>%
#   rename("COMID"=V1) %>% rownames_to_column(var = "StationCode")

# 03. GET UPSTREAM FLOWLINES --------------------------------------------------

# use list of gage NHD comids to make a list to pass to nhdplusTools to get flowlines
# pull data from 10 km upstream

## transform datum for flowlines
sel_usflow <- st_transform(sel_h12, crs=3310) # use CA Teale albs

# Use the GAGE com_list
coms_list <- map(sel_usflow$COMID, ~list(featureSource = "comid", featureID=.x))

# check
coms_list[[10]] # should list feature source and featureID

# Get upstream mainstem streamlines (10 km limit) from gages
mainstemsUS <- map(coms_list, ~navigate_nldi(nldi_feature = .x,
                                             mode="UM", # upstream main
                                             distance_km = 10))
beep(2)


# check length (for NAs?)
mainstemsUS %>%
  purrr::map_lgl(~ length(.x)>1) %>% table()

# drop NA/empty elements
mainstemsUS_c <- mainstemsUS %>% purrr::compact()

# make a single flat layer
mainstems_flat_us <- map_df(mainstemsUS_c, ~mutate(.x$UM_flowlines, comid_origin=.x$origin$comid, .after=nhdplus_comid))

# bind together
class(mainstems_flat_us)

# add direction to gage col
mainstems_us <- mainstems_us %>%
  mutate(from_gage = "UM")

# rm temp files
rm(mainstems_flat_us, mainstemsUS)

## Map and Save ---------------------------------

# preview
mapview(mainstems_us) +
  mapview(sel_bmi_gages_csci, cex=6, col.regions="orange",
          layer.name="Selected BMI Stations") +
  mapview(sel_gages_bmi, col.regions="skyblue", cex=7, color="blue2",
          layer.name="Selected USGS Gages")

# save
write_rds(mainstems_us, file = "data_output/02b_sel_gage_mainstems_us.rda")


# # make a single flat layer
# mainstems_flat_us <- mainstemsUS %>%
#   set_names(., sel_gages_algae$site_id) %>%
#   map2(sel_gages_algae$site_id, ~mutate(.x, gageID=.y))
