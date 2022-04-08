# square one


# Libraries ---------------------------------------------------------------

library(dplyr)
library(readr)
library(ggplot2)
library(sf)
library(nhdplusTools)
library(lwgeom) # get endpoint with lwgeom
library(mapview)
mapviewOptions(fgb=FALSE)

# Data --------------------------------------------------------------------

catch <- read_rds("data_input/catchments_final_lshasta.rds")
lsh_flowlines <- read_rds("data_input/final_flowlines_w_full_nhd_vaa.rds") %>%
  # remove all the junk just get what we need
  select(comid, fromnode, tonode, divergence, ftype,
         areasqkm, lengthkm, gnis_id, hydroseq)
# run get_tocomid?
lsh_fpath <- get_tocomid(lsh_flowlines)

# preview
mapview(catch, col.regions="gray") +
  mapview(lsh_fpath, zcol="hydroseq")

# Network Attributes ------------------------------------------------------

head(lsh_fpath <- select(sf::st_cast(lsh_fpath, "LINESTRING"),
                     -tonode, -fromnode, -divergence, -ftype))

mapview(lsh_fpath)


## Generate Sorted Network -------------------------------------------------

# from nhdPlus tutorial: https://usgs-r.github.io/nhdplusTools/articles/advanced_network.html
# sorts the flowpaths so headwaters come first and terminal flowpath last.
# Also produces a terminalID representing the outlet ID of the network. If multiple terminal networks had been provided, the terminalID allows us to group data by complete sub networks

# check and sort
head(lsh_fpath <- get_sorted(lsh_fpath, split = TRUE))

# add a temp hydroseq ID
lsh_fpath['hydrosequence'] <- seq(nrow(lsh_fpath), 1)
plot(lsh_fpath['hydrosequence'], key.pos = NULL)


## Generate Total Accumulated Upstream Flowpath (ArbolateSum) --------------

# Rename and compute weight
lsh_fpath[["arbolatesum"]] <- calculate_arbolate_sum(
  dplyr::select(lsh_fpath,
                ID = comid, toID = tocomid, length = lengthkm))

plot(sf::st_geometry(lsh_fpath), lwd = lsh_fpath$arbolatesum / 10)


# Now Get LevelPaths
# Get levelpaths
lsh_lp <- get_levelpaths(
  dplyr::select(lsh_fpath,
                ID = comid, toID = tocomid,
                nameID = gnis_id, weight = arbolatesum),
  status = FALSE, override_factor = 5)

# join back to dataset
head(lsh_fpath <- dplyr::left_join(lsh_fpath, lsh_lp, by = c("comid" = "ID")))

# look at topological plot
plot(lsh_fpath["topo_sort"], key.pos = NULL, reset = FALSE)
plot(lsh_fpath["levelpath"], key.pos = NULL)


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

flowlines <- navigate_nldi(nldi_feature = nldi_com,
                          mode = "upstreamTributaries",
                          distance_km = 200)

# filter to just the comids we want (above):
flowlines_filt <- flowlines$UT_flowlines %>%
  filter(nhdplus_comid %in% lsh_fpath$comid)

# map and see the very messed up stream network vs corrected
mapview(flowlines$origin, cex=5, col.regions="orange") +
  mapview(lsh_fpath, lwd=3.4, color="skyblue", layer.name="Revised LSH") +
  mapview(flowlines$UT_flowlines, lwd=0.5, color="darkblue", legend=FALSE, layer.name="NHD Original")


# What are NLDI Characteristics Avail? ------------------------------------

chars <- discover_nldi_characteristics()
View(chars$local)
View(chars$total)

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

DT::datatable(outlet_local)
knitr::kable(outlet_local)


# Download All Local Characteristics for Basin ----------------------------------

all_local <- sapply(lsh_fpath$comid, function(x, char) {
  chars <- get_nldi_characteristics(
    list(featureSource = "comid", featureID = as.character(x)),
    type = "local")

  filter(chars$local, characteristic_id == char)$characteristic_value

}, char = chars$local$characteristic_id)

local_characteristic <- data.frame(COMID = lsh_fpath$comid)
local_characteristic <- sapply(local_characteristic, as.numeric)

cat <- right_join(data$catchment, local_characteristic, by = c("FEATUREID" = "COMID"))



# Download Data in GPKG ---------------------------------------------------

# download all the extra VAA info
#subset_file <- tempfile(fileext = ".gpkg")
subset <- subset_nhdplus(comids = as.integer(flowlines$UT$nhdplus_comid),
                         output_file = "data_input/nhdplus_vaa.gpkg", # subset_file,
                         nhdplus_data = "download",
                         flowline_only = FALSE,
                         return_data = TRUE, overwrite = TRUE)

# pull out pieces
st_layers("data_input/nhdplus_vaa.gpkg")
flowline <- sf::st_read("data_input/nhdplus_vaa.gpkg", "NHDFlowline_Network")
catchment <- sf::st_read("data_input/nhdplus_vaa.gpkg", "CatchmentSP")
waterbody <- sf::st_read("data_input/nhdplus_vaa.gpkg", "NHDWaterbody")

# filter catchment to catch of interest:
catch_filt <- catchment %>% filter(featureid %in% lsh_fpath$comid)

# plot
plot(sf::st_geometry(catchment), col=alpha("gray30", 0.2), border=alpha("gray20",0.8), lwd=0.2)
plot(sf::st_geometry(waterbody), border="blue2", lwd=0.3, col = alpha("steelblue", 0.5), add = TRUE)
plot(sf::st_geometry(flowline), col = "blue", add=TRUE, lwd=0.7)
plot(st_endpoint(flowlines$origin$geometry), cex = 1.5, pch=21,  bg = "orange", add = TRUE)
plot(flowlines$origin$geometry, cex = 1.5, lwd = 2, col = "red", add = TRUE)

# get vaa?
# vaa <- get_vaa()
# names(vaa)
