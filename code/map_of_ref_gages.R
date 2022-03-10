# map of ref gages
library(sf)
library(mapview)
mapviewOptions(fgb = FALSE)
library(tidyverse)



# Data --------------------------------------------------------------------

refgages <- read_csv("data/model_training/FFM_model_ref_gages.csv") %>%
  st_as_sf(coords=c("LONGITUDE","LATITUDE"), crs=4326, remove=FALSE)

mapview(refgages)
