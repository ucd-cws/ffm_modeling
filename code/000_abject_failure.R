# square one...again


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(sf)
library(nhdplusTools)
library(lwgeom) # get endpoint with lwgeom
library(mapview)
mapviewOptions(fgb=FALSE)

# Data --------------------------------------------------------------------

# catchments (revised)
catch <- read_rds("data_input/catchments_final_lshasta.rds")
# huc10
h10 <- read_rds("data_input/huc10_little_shasta.rds")
gages <- read_rds("data_input/gages_little_shasta.rds")
springs <- read_rds("data_input/springs_little_shasta.rds")
lsh_fpath <- read_rds("data_input/final_flowlines_w_full_nhd_vaa.rds") %>%
  # remove all the junk just get what we need
  select(comid, fromnode, tonode, divergence, ftype,
         areasqkm, lengthkm, gnis_id, hydroseq) %>%
  st_transform(4269)
# run get_tocomid?
lsh_fpath <- get_tocomid(lsh_fpath)

# preview
mapview(catch, col.regions="gray") +
  mapview(lsh_fpath, zcol="hydroseq", legend=FALSE, layer.name="Flowlines") +
  mapview(springs, color="yellow") +
  mapview(gages, col.regions="black", cex=2.5)


# Need to Clean Up Catchments: --------------------------------------------

# drop these gridcodes, they are all splinters
to_drop <- c(1386713,1387823, 1387701,1387917, 1387877,
             1387655, 1387682, 1387926, 1387559, 1387104,
             1387360, 1387795,1387830, 1520905, 1386208,1386450,
             1386396,1386475, 1386300,1386459,1386291, 1386638, 1386532,
             1386593, 1386796, 1387661, 1387838, 1387679, 1386893,
             1386339, 1387033)
# note 1387724 and 1387816 have some slivers/edges

catch_clean <- catch %>% filter(!GRIDCODE %in% to_drop)

# preview
mapview(catch_clean, zcol="GRIDCODE", legend = FALSE) +
  mapview(catch_clean, zcol="comid", legend=FALSE) +
  mapview(lsh_fpath, color="steelblue", legend=FALSE, layer.name="Flowlines") +
  mapview(springs, color="yellow") +
  mapview(gages, col.regions="black", cex=2.5)


# Network Attributes ------------------------------------------------------

# make sure everything is a linestring
lsh_fpath <- select(sf::st_cast(lsh_fpath, "LINESTRING"),
                     -tonode, -fromnode, -divergence, -ftype)

## Generate Sorted Network -------------------------------------------------

# from nhdPlus tutorial: https://usgs-r.github.io/nhdplusTools/articles/advanced_network.html
# sorts the flowpaths so headwaters come first and terminal flowpath last.
# Also produces a terminalID representing the outlet ID of the network. If multiple terminal networks had been provided, the terminalID allows us to group data by complete sub networks

# check and sort
lsh_fpath <- get_sorted(lsh_fpath, split = TRUE)

# add a temp hydroseq ID
lsh_fpath['hydrosequence'] <- seq(nrow(lsh_fpath), 1)

## Generate Total Accumulated Upstream Flowpath (ArbolateSum) --------------

# Rename and compute weight
lsh_fpath[["arbolatesum"]] <- calculate_arbolate_sum(
  dplyr::select(lsh_fpath,
                ID = comid, toID = tocomid, length = lengthkm))

plot(sf::st_geometry(lsh_fpath), lwd = lsh_fpath$arbolatesum / 10)

# # Now Get LevelPaths
# lsh_lp <- get_levelpaths(
#   dplyr::select(lsh_fpath,
#                 ID = comid, toID = tocomid,
#                 nameID = gnis_id, weight = arbolatesum),
#   status = FALSE, override_factor = 5)
#
# # join back to dataset
# head(lsh_fpath <- dplyr::left_join(lsh_fpath, lsh_lp, by = c("comid" = "ID")))
#
# # look at topological plot
# plot(lsh_fpath["topo_sort"], key.pos = NULL, reset = FALSE)
# plot(lsh_fpath["levelpath"], key.pos = NULL)

# Using NLDI --------------------------------------------------------------

# see here: https://waterdata.usgs.gov/blog/nldi_update/

## Starting COMID ----------------------------------------------------------

# downstream COMID: 3917946
start_comid <- 3917946
# downstream HYDROSEQ: 10020426

# if unknown but have X/Y:
# start_point <- st_sfc(st_point(c(-89.362239, 43.090266)), crs = 4269)
# start_comid <- discover_nhdplus_id(start_point)

# setup feature (can use USGS site: nwissite, or comid)
nldi_com <- list(featureSource = "comid",
                     featureID = start_comid) # plug in start_comid here

## GET JUST FLOWLINES AND PLOT --------------------------------------------------

# get JUST the raw NHD flowlines (screwed up)
flowlines_full <- navigate_nldi(nldi_feature = nldi_com,
                          mode = "upstreamTributaries",
                          distance_km = 200)

# filter to just the comids we want (above):
flowlines_filt <- flowlines_full$UT_flowlines %>%
  filter(nhdplus_comid %in% lsh_fpath$comid)

# map and download all the things
# data <- plot_nhdplus(nldi_com, flowline_only = FALSE)

# get huc8 (nhdplusTools): can use id or AOI (sf point)
#lsh_h8 <- get_huc8(AOI = flowlines_full$origin, id = NULL, t_srs = 4269, buffer = 0.5)
#lsh_h12 <- get_huc12(AOI = flowlines_full$origin, id = NULL, t_srs = 4269, buffer = 0.5)
#lsh_basin <- get_nldi_basin(nldi_com)

# map and see the very messed up stream network vs corrected
mapview(flowlines_full$origin, cex=5, col.regions="orange") +
  mapview(h10, color="darkblue",lwd=3, col.regions="skyblue", alpha.regions=0.5)+
  mapview(catch_clean, col.regions="orange", alpha.regions=0.2)+
  mapview(lsh_basin, col.regions="gray", alpha.regions=0.5)+
  mapview(lsh_fpath, lwd=3.4, color="skyblue", layer.name="Revised LSH") +
  mapview(flowlines_full$UT_flowlines, lwd=0.5, color="darkblue", legend=FALSE, layer.name="NHD Original")

## Download FULL FLOWLINE VAA in GPKG ---------------------------------------------------

# download all the extra VAA info
#subset_file <- tempfile(fileext = ".gpkg")
# grab based on COMIDs from catchments (could use lsh_fpath$comids)
nhd_vaa <- subset_nhdplus(comids = as.integer(catch$FEATUREID),
                         output_file = "data_input/nhdplus_vaa.gpkg", # subset_file,
                         nhdplus_data = "download",
                         flowline_only = FALSE,
                         return_data = TRUE, overwrite = TRUE)

# pull out pieces
st_layers("data_input/nhdplus_vaa.gpkg")
flowlines_vaa <- sf::st_read("data_input/nhdplus_vaa.gpkg", "NHDFlowline_Network") %>%
  filter(comid %in% lsh_fpath$comid)
catchment_vaa <- sf::st_read("data_input/nhdplus_vaa.gpkg", "CatchmentSP")
waterbody_vaa <- sf::st_read("data_input/nhdplus_vaa.gpkg", "NHDWaterbody")

mapview(catchment_vaa, lwd=4, color="gray") +
  mapview(h10, color="darkblue",lwd=3, col.regions="skyblue", alpha.regions=0) +
  mapview(catch_clean, lwd=1, color="orange", alpha.regions=0) +
  mapview(flowlines_vaa, lwd=1.5, color="steelblue", layer.name="Revised LSH") +
  mapview(loi, color="red")


# filter catchment to catch of interest (spatial join)
catch_filt <- sf::st_join(x = st_transform(catchment_vaa, 3310), y = catch, join=st_contains_properly, left=TRUE) %>% st_transform(4269)
mapview(catchment_vaa) + mapview(catch_filt, col.regions="yellow")
catch_filt <- catchment_vaa %>% filter(gridcode %in% catch$GRIDCODE)

# plot
plot(sf::st_geometry(catchment_vaa), col=alpha("gray30", 0.1), border=alpha("tan3",0.5), lwd=0.2)
plot(sf::st_geometry(catch_filt), col=alpha("gray10", 0.2), border=alpha("brown2",0.5), lwd=0.2, add=TRUE)
plot(sf::st_geometry(waterbody_vaa), border="blue2", lwd=0.3, col = alpha("steelblue", 0.5), add = TRUE)
plot(sf::st_geometry(lsh_fpath), col = "blue", add=TRUE, lwd = lsh_fpath$arbolatesum / 15)
plot(sf::st_geometry(flowlines_vaa), col = alpha("steelblue", 0.5), add=TRUE, lwd=0.7)
plot(st_endpoint(flowlines_full$origin$geometry), cex = 1.5, pch=21,  bg = "orange", add = TRUE)
plot(flowlines_full$origin$geometry, cex = 1.5, lwd = 2, col = "red", add = TRUE)



# What are NLDI Characteristics Avail? ------------------------------------

chars <- discover_nldi_characteristics()
# View(chars$local)
# View(chars$total)

# Download the NLDI Catch Info --------------------------------------------

outlet_local <- get_nldi_characteristics(nldi_com, type = "local")

outlet_local <- left_join(outlet_local$local, chars$local,
                          by = "characteristic_id")

outlet_local <- outlet_local %>%
  select(ID = characteristic_id,
         Description = characteristic_description,
         Value = characteristic_value,
         Units = units,
         link = dataset_url) %>%
  mutate(link = paste0('<a href="', link, '">link</a>'))

#DT::datatable(outlet_local)
#knitr::kable(outlet_local)

# names(chars)
# View(chars$total)
# View(chars$divergence_routed)

# Download All Local Characteristics for Basin ----------------------------------

all_local <- sapply(lsh_fpath$comid, function(x, char) {
  chars <- get_nldi_characteristics(
    list(featureSource = "comid", featureID = as.character(x)),
    type = "local")
  filter(chars$local, characteristic_id == char)$characteristic_value
}, char = chars$local$characteristic_id)

all_local_df <- as.data.frame(all_local) %>%
  mutate(across(everything(), as.numeric)) %>%
  t(.) %>% as_tibble(.name_repair = "minimal") %>%
  purrr::set_names(nm=chars$local$characteristic_id) %>%
  mutate(COMID = lsh_fpath$comid, .before=1)

# GET CROSSWALK --------------------------------------------------

xwalk <- read_csv("data_input/model_application/input_var_names_xwalk.csv") %>%
  mutate(varscap = toupper(examp))

# GET UNDERLYING CATCH DATA (PRE ACCUMULATION) ----------------------------

catch_dat <- read_csv("https://github.com/ryanpeek/ffm_accumulation/blob/main/data_clean/07_final_catchment_data_for_accum.csv?raw=true")

# Create Networked Flowlines w VAA -----------------------------------------

# check and sort
flowlines_vaa <- get_tocomid(flowlines_vaa)
flowlines_vaa <- get_sorted(flowlines_vaa, split = TRUE)

# add a temp hydroseq ID
flowlines_vaa['hydrosequence'] <- seq(nrow(flowlines_vaa), 1)

# Rename and compute weight for arbolatesum
flowlines_vaa[["arbolatesum"]] <- calculate_arbolate_sum(
  dplyr::select(flowlines_vaa,
                ID = comid, toID = tocomid, length = lengthkm))

plot(sf::st_geometry(flowlines_vaa), lwd = lsh_fpath$arbolatesum / 10)

# DROP SINKS
# drop sinks (isolated channels)
sinks <- c(3917228, 3917212, 3917214, 3917218, 3917220,
           3917960, 3917958, 3917276, 3917278, 3917274,
           3917282, 3917284, 3917286, 3917280, 3917268,
           3917256, 3917250, 3917272, 3917956)
flowlines_vaa_trim <- flowlines_vaa %>% filter(!comid %in% sinks)

# quick map
mapview(lsh_fpath, color="gray", legend=FALSE, layer.name="Flowlines", lwd=6) +
  mapview(flowlines_vaa_trim %>% select(comid:id, streamleve:dnhydroseq),
          lwd=1, zcol="hydroseq", legend=FALSE)


# using nhdplusTools function here to generate network and run accumulation
net <- prepare_nhdplus(flowlines_vaa_trim, 0, 0, 0, purge_non_dendritic = FALSE, warn = FALSE)


# ACCUMULATION ------------------------------------------------------------

net <- select(net, ID = COMID, toID = toCOMID) %>%
  left_join(select(st_drop_geometry(flowlines_vaa_trim), COMID=comid, AreaSqKM=areasqkm),
            by = c("ID" = "COMID")) %>%
  left_join(catch_dat, by = c("ID" = "comid"))

# accum over a single var for now:
characteristic <- "cat_pet"
net[["temp_col"]] <- net[[characteristic]] * net$AreaSqKM

net[["tot_cat_pet"]] <- nhdplusTools:::accumulate_downstream(net, "temp_col")
net$DenTotDASqKM <- nhdplusTools:::accumulate_downstream(net, "AreaSqKM")

net[["tot_cat_pet"]] <- net[["tot_cat_pet"]] / net$DenTotDASqKM

cat_accum <- right_join(catch_clean,
                  select(net, -temp_col, -toID, -DenTotDASqKM),
                  by = c("comid" = "ID")) %>% st_transform(4269)

st_crs(cat_accum)==st_crs(flowlines_vaa_trim)
plot(cat_accum["tot_cat_pet"], reset = FALSE)
plot(st_geometry(flowlines_vaa_trim), add = TRUE,
     lwd = flowlines_vaa_trim$streamorde, col = "lightblue")

# check
filter(all_local_df, COMID == 3917198)["CAT_PET"]
filter(cat_accum, comid == 3917198)[["cat_pet"]]

