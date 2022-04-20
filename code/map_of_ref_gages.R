# map of ref gages
library(sf)
library(mapview)
mapviewOptions(fgb = FALSE)
library(tidyverse)



# Data --------------------------------------------------------------------

refgages <- read_csv("data_input/model_training/FFM_model_ref_gages.csv") %>%
  st_as_sf(coords=c("LONGITUDE","LATITUDE"), crs=4326, remove=FALSE)

mapview(refgages)


# hatcreek: 7952754 (T11355500)
# Sac at Mt Shasta: 7964867 (T11341400)
