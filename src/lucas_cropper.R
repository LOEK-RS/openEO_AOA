
library(sf)
library(raster)
library(tidyverse)


lucas = fread("data/lucas/GRID_CSVEXP_20171113.csv")
lucas = st_as_sf(lucas, coords = c("X_WGS84", "Y_WGS84"), crs = 4326)


aoi = list(          west = 10.324727539019404,
                     south = 51.299651394758726,
                     east = 10.60392084289973,
                     north = 51.416092203960005)


#aoi <- list(west = 10.4005, south = 51.3371, east = 10.5152, north = 51.3856) # niederorschel
aoi2 = st_as_sf(st_as_sfc(st_bbox(extent(c(aoi$west, aoi$east, aoi$south, aoi$north)))), crs = 4326)

mapview(aoi2)

lucas = st_crop(lucas, aoi2)
lucas = lucas %>% select("POINT_ID")


saveRDS(lucas, "data/lucas/lucas_aoa_ID.rds")



