# function to download SciBase files of interest


# Libraries ---------------------------------------------------------------

library(sbtools)
library(glue)
library(here)
library(fs)
#library(xml2)
options(tidyverse.quiet = TRUE)
library(tidyverse)
options(scipen = 100)


# To Get DOI --------------------------------------------------------------

# query using DOI
# Select Attributes: https://doi.org/10.5066/F7765D7V.
# doi_info <- query_sb_doi('10.5066/F7765D7V', limit = 5000)
# (id_item <- doi_info[[1]]$id) # item ID: 5669a79ee4b08895842a1d47

id_item <- "5669a79ee4b08895842a1d47"
# get info for item:
# id_item_get <- item_get(id_item)
# names(id_item_get)

## get list of just the main files (recursive = FALSE)
# main_files <- item_list_files(id_item, recursive = FALSE)
# main_files <- main_files %>%
#   mutate(size_mb = size * 1e-6, .after=size)
# main_files %>% select(1:3)

## list all files (takes a min)
# all_files <- item_list_files(id_item,recursive = TRUE)
# all_files <- all_files %>%
#   mutate(size_mb = size * 1e-6, .after=size)
# dim(all_files) # 1412 files!
# all_files %>% select(1:3) %>% arrange(desc(size_mb)) %>% head(n=10)
# write_rds(all_files, file = here("data_input/sb_file_list_doi_10.5066_F7765D7V.rds"))

# Load Data ---------------------------------------------------------------


all_files <- read_rds(here("data_input/sb_file_list_doi_10.5066_F7765D7V.rds"))

# get xwalk with files of interest
xwalk <- read_csv("data_input/08_accumulated_final_xwalk.csv")


# filter to filenames of interest
sb_files_to_get <- all_files %>%
  filter(grepl("^PPT[0-9]{4}_CAT_CONUS", fname)|
           grepl("^TAV[0-9]{4}_CAT_CONUS", fname)|
           grepl("^RUN[0-9]{4}_CAT_CONUS", fname)|
           fs::path_ext_remove(fname) %in% fs::path_ext_remove(xwalk$source_file))

# Setup Dir for Download --------------------------------------------------


# download dir
dl_dir <- "data_input/scibase_nhd"

# clean dir
if (fs::dir_exists(here(dl_dir))) {
  glue::glue("Directory {here(dl_dir)} exists!")
} else {
  fs::dir_create(here(dl_dir))
  glue::glue("Directory {dl_dir} created")
}

# now download (this can take awhile)
# not including the nhd xml and node files (see all_files)
library(tictoc)
tic()
map2(sb_files_to_get$url, sb_files_to_get$fname,
     ~ if(!file.exists(glue("{here(dl_dir)}/{.y}"))){
       download.file(.x, glue("{here(dl_dir)}/{.y}"))}
     else(print("File exists")))
beepr::beep(2)
toc()

# about 10 min vs. >1 hr!

# DOWNLOAD KRUG DATA -----------------------------------------------------------

# krug
download.file("https://water.usgs.gov/GIS/dsdl/runoff.e00.zip",
              destfile = glue("{here(dl_dir)}/krug_runoff_avg_ann_1951-1980.e00.zip"))

# unzip and make KRUG RUNOFF
unzip(glue("{here(dl_dir)}/krug_runoff_avg_ann_1951-1980.e00.zip"),
      exdir = glue("{here(dl_dir)}"))

# rename
fs::file_move(path = glue("{here(dl_dir)}/runoff.e00"),
              new_path = glue("{here(dl_dir)}/krug_runoff_avg_ann_1951-1980.e00"))



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
