# download KRUG_RUNOFF Data

# PROCESS KRUG ------------------------------------------------------------

library(tidyverse)
library(sf)
library(glue)
library(here)
library(mapview)
mapviewOptions(fgb = FALSE)
library(rmapshaper)
library(lwgeom)

# download dir
dl_dir <- "data_input/scibase_nhd"

# READ IN DATA ------------------------------------------------------------

# read in krug
krug <- st_read(glue("{here(dl_dir)}/krug_runoff_avg_ann_1951-1980.e00")) %>%
  st_transform(5070)

# get catchments
catch_vaa <- sf::st_read(here("data_input/nhdplus_vaa.gpkg"), "CatchmentSP", quiet=TRUE) %>% st_transform(st_crs(krug))
# create a dissolved boundary:
catch_diss <- ms_dissolve(catch_vaa)

# get ca boundary
ca <- urbnmapr::get_urbn_map("states", sf=TRUE) %>%
  filter(state_abbv =="CA") %>%
  st_transform(5070)

# crop to ca
ca_krug <- st_intersection(krug, ca)

# split by ca krug
catch_krug <- lwgeom::st_split(catch_diss, ca_krug) %>%
  st_collection_extract(c("POLYGON")) %>%
  mutate(rowid = 1:nrow(.)) %>%
  slice(1:2) %>%
  st_join(ca_krug, join=st_touches) %>%
  mutate(INCHES = ifelse(is.na(INCHES), 5, 10))

#mapview(catch_krug, zcol="INCHES") + mapview(ca_krug, zcol="INCHES")

# get flowlines
lsh_flowline <- read_rds(here("data_input/final_flowlines_w_full_nhd_vaa.rds")) %>%
  select(comid, areasqkm, lengthkm, gnis_id, hydroseq) %>%
  st_transform(st_crs(krug))

# join flowlines
krug_comids <- st_join(lsh_flowline, catch_krug, join=st_nearest_feature) #%>%

# preview
#st_drop_geometry(krug_comids) %>% group_by(comid) %>% tally()
#mapview(krug_comids, zcol="INCHES") + mapview(catch_krug, zcol="INCHES")

# make csv of this
krug_runoff_csv <- krug_comids %>% st_drop_geometry() %>%
  select(COMID=comid, krug_runoff = INCHES) %>%
  mutate(source = "krug_runoff_avg_ann_1951-1980.e00")

# write to data_clean
fs::dir_create(here("data_output/nhd_catchdat"))
write_csv(krug_runoff_csv, file = here("data_output/nhd_catchdat/KRUG_RUNOFF.csv"))
