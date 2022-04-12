# square one...again

# LIBRARIES ---------------------------------------------------------------

library(tidyverse)
library(sf)
library(nhdplusTools)
library(lwgeom) # get endpoint with lwgeom
library(mapview)
library(units)
mapviewOptions(fgb=FALSE)

# DATA --------------------------------------------------------------------

# catchments (revised)
catch <- read_rds("data_input/catchments_final_lshasta.rds")
h10 <- read_rds("data_input/huc10_little_shasta.rds")
gages <- read_rds("data_input/gages_little_shasta.rds")
springs <- read_rds("data_input/springs_little_shasta.rds")

# flowlines
lsh_fpath <- read_rds("data_input/final_flowlines_w_full_nhd_vaa.rds") %>%
  # remove all the junk just get what we need
  select(comid, fromnode, tonode, divergence, ftype,
         areasqkm, lengthkm, gnis_id, hydroseq) %>%
  st_transform(4269)

lois <- lsh_fpath %>%
  filter(comid %in% c(3917946, 3917950, 3917198))

## Clean Map -------------------------------------------------------

# preview
mapview(catch, col.regions="gray") +
  mapview(lsh_fpath, zcol="hydroseq", legend=FALSE, layer.name="Flowlines") +
  mapview(springs, col.regions="steelblue", alpha.regions=0.8, cex=4) +
  mapview(gages, col.regions="black", cex=2.5) +
  mapview(st_endpoint(lois), col.regions="orange", cex=6)


# TIDY --------------------------------------------------------------------

# need to drop slivers from boundary clip using HUC10. This does change areas
# of a few catchments because evidently the catchments and HUC10 were not nested
# using old messed up hydrodelineation.

## TIDY: Clean Up Catchments: --------------------------------------------

# drop these gridcodes, they are all splinters
to_drop <- c(1386713,1387823, 1387701,1387917, 1387877,
             1387655, 1387682, 1387926, 1387559, 1387104,
             1387360, 1387795,1387830, 1520905, 1386208,1386450,
             1386396,1386475, 1386300,1386459,1386291, 1386638, 1386532,
             1386593, 1386796, 1387661, 1387838, 1387679, 1386893,
             1386339, 1387033)
# note 1387724 and 1387816 have some slivers/edges
# run with and without !GRIDCODE to see
catch_clean <- catch %>% filter(!GRIDCODE %in% to_drop) %>%
  # recalc areas?
  mutate(area2 = units::set_units(st_area(geom),"m^2") %>% set_units("km^2") %>% drop_units())


# preview (good!)
mapview(catch_clean, zcol="GRIDCODE", legend = FALSE) +
  mapview(catch_clean, zcol="comid", legend=FALSE) +
  mapview(lsh_fpath, color="skyblue", legend=FALSE, layer.name="Flowlines") +
  mapview(springs, col.regions="steelblue") +
  mapview(gages, col.regions="black", cex=2.5)

# note the stepped jagged edges of the catchments....
# not sure where/why they are so stepped??

## TIDY: Clean Up and Drop Sinks -------------------------------------------------

# now make a flowline version that drops sinks in southern part of watershed

# drop sinks (isolated channels)
sinks <- c(3917228, 3917212, 3917214, 3917218, 3917220,
           3917960, 3917958, 3917276, 3917278, 3917274,
           3917282, 3917284, 3917286, 3917280, 3917268,
           3917256, 3917250, 3917272, 3917956)
lsh_fpath_trim <- lsh_fpath %>% filter(!comid %in% sinks)

# quick map
mapview(lsh_fpath, color="gray", legend=FALSE, layer.name="Flowlines", lwd=6) +
  mapview(lsh_fpath_trim, lwd=1, zcol="hydroseq", legend=FALSE, layer.name="Selected Flowlines") +
  mapview(st_endpoint(lois), col.regions="orange", layer.name="LOI D/S")


# FLOWLINE NETWORK -----------------------------------------------

# next need to generate a clean flowline network

## Convert Network to Linestring ---------------------------------------

# convert everything is a linestring
#map(lsh_fpath_trim$geom, ~sf::st_geometry(.x)[[1]])
lsh_fpath_trim_lstring <- sf::st_cast(lsh_fpath_trim, "LINESTRING")
#map(lsh_fpath_trim_lstring$geom, ~sf::st_geometry(.x)[[1]])

# generate clean comid network
lsh_comidnet <- get_tocomid(lsh_fpath_trim_lstring, return_dendritic = TRUE, missing = 0, add = TRUE)

# regenerate/check flowline lengths:
lsh_comidnet <- lsh_comidnet %>%
  mutate(lengthkm_check = st_length(.)) %>%
  mutate(lengthkm_check = round(x = units::drop_units(lengthkm_check)/1000, 3))

# compare
#select(lsh_comidnet, starts_with("length")) %>% View()

## Generate Sorted Network -------------------------------------------------

# from nhdPlus tutorial: https://usgs-r.github.io/nhdplusTools/articles/advanced_network.html
# sorts the flowpaths so headwaters come first and terminal flowpath last.
# Also produces a terminalID representing the outlet ID of the network. If multiple terminal networks had been provided, the terminalID allows us to group data by complete sub networks

# check and sort
lsh_flownet <- get_sorted(lsh_comidnet, split = FALSE)
lsh_flownet['sort_order'] <- 1:nrow(lsh_flownet)
#plot(lsh_flownet['sort_order'])
mapview(lsh_flownet, zcol="sort_order")
# add a temp hydroseq ID
# lsh_flownet['hydrosequence'] <- seq(nrow(lsh_flownet), 1)

## Generate Total Accumulated Upstream Flowpath (ArbolateSum) --------------

# Rename and compute weight for accumulated flowpath
lsh_flownet[["arbolatesum"]] <- calculate_arbolate_sum(
  dplyr::select(lsh_flownet,
                ID = comid, toID = tocomid, length = lengthkm_check))

# plot based on upstream flowpath
plot(sf::st_geometry(lsh_flownet), lwd = lsh_flownet$arbolatesum / 10)

# Make LEVEL Paths --------------------------------------------------------

# # Now Get LevelPaths
lsh_lp <- get_levelpaths(
  dplyr::select(lsh_flownet,
                ID = comid, toID = tocomid,
                nameID = gnis_id, weight = arbolatesum),
  status = FALSE, override_factor = 5)

## join back to dataset
lsh_flownet <- dplyr::left_join(lsh_flownet, lsh_lp, by = c("comid" = "ID"))

# look at topological plot
plot(lsh_flownet["topo_sort"], key.pos = NULL, reset = FALSE)

# look at level plot
plot(lsh_flownet["levelpath"], key.pos = NULL)

# USING NLDI/NHDTOOLS --------------------------------------------------------------

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

## Get NHD Flowlines -----------------------------------------------

# get JUST the raw NHD flowlines (screwed up)
flowlines_full <- navigate_nldi(nldi_feature = nldi_com,
                          mode = "upstreamTributaries",
                          distance_km = 200)

# filter to just the comids we want (above):
flowlines_filt <- flowlines_full$UT_flowlines %>%
  filter(nhdplus_comid %in% lsh_fpath_trim$comid)

plot(flowlines_filt$geometry)

# map and download all the things
# data <- plot_nhdplus(nldi_com, flowline_only = FALSE)

## Get HUC8 or Basin ----------------------------------------------------

# these are incorrect for the most part so cannot use, but code available
# get huc8 (nhdplusTools): can use id or AOI (sf point)
#lsh_h8 <- get_huc8(AOI = flowlines_full$origin, id = NULL, t_srs = 4269, buffer = 0.5)
#lsh_h12 <- get_huc12(AOI = flowlines_full$origin, id = NULL, t_srs = 4269, buffer = 0.5)
lsh_basin <- get_nldi_basin(nldi_com)

# map and see the very messed up stream network vs corrected
mapview(h10, color="darkblue",lwd=3, col.regions="skyblue", alpha.regions=0.5)+
  mapview(catch_clean, col.regions="orange", alpha.regions=0.2, layer.name="Cleaned Catchments")+
  mapview(lsh_basin, col.regions="gray", alpha.regions=0.5, layer.name="NHD Basin")+
  mapview(lsh_fpath_trim, lwd=3.4, color="skyblue", layer.name="Revised LSH") +
  mapview(flowlines_full$UT_flowlines, lwd=0.5, color="darkblue", legend=FALSE, layer.name="NHD Original")

## Get NHD_VAA in GPKG ---------------------------------------------------

# download all the extra VAA info
#subset_file <- tempfile(fileext = ".gpkg")
# grab based on COMIDs from catchments (could use lsh_fpath$comids)
nhd_vaa <- subset_nhdplus(comids = as.integer(catch_clean$FEATUREID),
                         output_file = "data_input/nhdplus_vaa.gpkg", # subset_file,
                         nhdplus_data = "download",
                         flowline_only = FALSE,
                         return_data = TRUE, overwrite = TRUE)

# pull out pieces
st_layers("data_input/nhdplus_vaa.gpkg")

# Read in NHD Flowline/Catch VAA ------------------------------------------------

# get nhd flowlines
nhd_vaa <- sf::st_read("data_input/nhdplus_vaa.gpkg", "NHDFlowline_Network") %>%
  filter(comid %in% lsh_fpath_trim$comid)

# get nhd catchments
catch_vaa <- sf::st_read("data_input/nhdplus_vaa.gpkg", "CatchmentSP")

# preview
mapview(catch_vaa, lwd=6, color="gray", col.regions="gray", alpha.regions=0.3, layer.name="NHD Catch") +
  #mapview(h10, color="darkblue",lwd=3, col.regions="skyblue", alpha.regions=0) +
 mapview(catch_clean, lwd=0.7, color="orange", alpha.regions=0, col.regions="orange") +
  mapview(nhd_vaa, lwd=1.5, color="steelblue", layer.name="Revised LSH") +
  mapview(lois, color="red")

# join catch with the flownet
catch_vaa <- left_join(catch_vaa,
                       st_drop_geometry(lsh_flownet) %>%
                         select(comid:divergence, hydroseq, sort_order:levelpath), by=c("featureid"="comid"))

# need to recalculate totdasqkm (TotDASqKM)
catch_area <- select(nhd_vaa, comid, areasqkm) %>%
  right_join(., prepare_nhdplus(nhd_vaa, 0, 0,
                                purge_non_dendritic = FALSE,
                             warn = FALSE),
             by = c("comid"="COMID")) %>%
  select(ID = comid, toID = toCOMID, area = areasqkm)

new_da <- calculate_total_drainage_area(catch_area)

catch_area$totda <- new_da
catch_area$nhdptotda <- nhd_vaa$totdasqkm

# replace the new_da in the nhd_vaa
nhd_vaa <- nhd_vaa %>% select(-c(totdasqkm)) %>%
  left_join(st_drop_geometry(catch_area), by=c("comid"="ID")) %>%
  select(-c(toID)) %>% rename(totdasqkm = totda) %>%
  relocate(totdasqkm, .after="areasqkm")


# plot
plot(sf::st_geometry(catch_vaa), col=alpha("gray30", 0.1), border=alpha("tan3",0.8), lwd=0.9)
plot(catch_vaa["sort_order"], reset = FALSE, add=TRUE)
plot(sf::st_geometry(lsh_flownet), col = "blue", add=TRUE, lwd = lsh_flownet$arbolatesum / 15)
plot(sf::st_geometry(nhd_vaa), col = alpha("skyblue", 0.8), add=TRUE, lwd=0.7)
plot(st_endpoint(lois), pch=21, cex = 1.5, bg = "gray70", add = TRUE)

## What are NLDI Characteristics Avail? ------------------------------------

chars <- discover_nldi_characteristics()
# View(chars$local)
# View(chars$total)

## NLDI Catchment Data --------------------------------------------

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

## Download All Local Characteristics for Basin ----------------------------------

all_local <- sapply(lsh_flownet$comid, function(x, char) {
  chars <- get_nldi_characteristics(
    list(featureSource = "comid", featureID = as.character(x)),
    type = "local")
  filter(chars$local, characteristic_id == char)$characteristic_value
}, char = chars$local$characteristic_id)

all_local_df <- as.data.frame(all_local) %>%
  mutate(across(everything(), as.numeric)) %>%
  t(.) %>% as_tibble(.name_repair = "minimal") %>%
  purrr::set_names(nm=chars$local$characteristic_id) %>%
  mutate(COMID = lsh_fpath_trim$comid, .before=1)

# PREP ACCUMULATION ---------------------------------------------------

## get crosswalk --------------------------------------------------

# cross of accumulation data
xwalk <- read_csv("data_input/model_application/input_var_names_xwalk.csv") %>%
  mutate(varscap = toupper(examp))

## Get Underlying Catchmnet Data for Accumulation ----------------------------

catch_dat <- read_csv("https://github.com/ryanpeek/ffm_accumulation/blob/main/data_clean/07_final_catchment_data_for_accum.csv?raw=true")

## Create Networked Flowlines w VAA -----------------------------------------

# check and sort
nhd_vaa <- get_tocomid(nhd_vaa, return_dendritic = TRUE, add = TRUE)
nhd_vaa <- get_sorted(nhd_vaa, split = TRUE)

# add a temp hydroseq ID
nhd_vaa['hydrosequence'] <- seq(nrow(nhd_vaa), 1)

# Rename and compute weight for arbolatesum
nhd_vaa[["arbolatesum"]] <- calculate_arbolate_sum(
  dplyr::select(nhd_vaa,
                ID = comid, toID = tocomid, length = lengthkm))

# update drainage area
plot(sf::st_geometry(nhd_vaa), lwd = nhd_vaa$arbolatesum / 10)

# using nhdplusTools function here to generate network and run accumulation

lsh_net <- prepare_nhdplus(nhd_vaa, 0, 0, 0, purge_non_dendritic = FALSE, warn = FALSE)

# NOTE need to update AREASKM here!!!!!!!

# RUN ACCUMULATION -------------------------------------------------------

lsh_accum <- select(lsh_net, ID = COMID, toID = toCOMID) %>%
  left_join(select(st_drop_geometry(nhd_vaa),
                   COMID=comid, AreaSqKM=areasqkm),
            by = c("ID" = "COMID")) %>%
  left_join(catch_dat, by = c("ID" = "comid"))

# accum over a single var for now:
characteristic <- "cat_pet"
lsh_accum[["temp_col"]] <- lsh_accum[[characteristic]] * lsh_accum$AreaSqKM

lsh_accum[["tot_cat_pet"]] <- nhdplusTools:::accumulate_downstream(lsh_accum, "temp_col")
lsh_accum$DenTotDASqKM <- nhdplusTools:::accumulate_downstream(lsh_accum, "AreaSqKM")

lsh_accum[["tot_cat_pet"]] <- lsh_accum[["tot_cat_pet"]] / lsh_accum$DenTotDASqKM

cat_accum <- right_join(catch_clean,
                  select(lsh_accum, -temp_col, -toID, -DenTotDASqKM),
                  by = c("comid" = "ID")) %>% st_transform(4269)

st_crs(cat_accum)==st_crs(nhd_vaa)

mapview(cat_accum, zcol="tot_cat_pet")

plot(cat_accum["tot_cat_pet"], reset = FALSE)
plot(st_geometry(nhd_vaa), add = TRUE,
     lwd = nhd_vaa$streamorde, col = "lightblue")

# check
filter(all_local_df, COMID == 3917198)["CAT_PET"]
filter(cat_accum, comid == 3917198)[["cat_pet"]]

