library(data.table)
library(sf)
library(mapview)
library(tidyverse)

unzip("data/lucas/GRID_CSVEXP_20171113.zip", exdir = "data/lucas")
# setwd("/home/petra/openEO/iloek/openEO_AOA")
lucas = fread("data/lucas/GRID_CSVEXP_20171113.csv")
lucas = st_as_sf(lucas, coords = c("X_WGS84", "Y_WGS84"), crs = 4326)
lucas


# look at a small sample of the data
mapview(lucas[sample(nrow(lucas), 10000),],
        zcol = "NUTS0_13", layer.name = "Country", cex = 3)


# look at germany only

lucas_germany = lucas %>% filter(NUTS0_13 == "DE")
lucas_germany = left_join(lucas_germany, luts$strata2018, by = "STR18")


mapview(lucas_germany[sample(nrow(lucas_germany), 10000),],
        zcol = "class18", cex = 3, popup = FALSE, layer.name = "Landcover")

# look at class distribution
table(lucas_germany$class18)

# export AOA to file
lucas_aoa <- st_crop(lucas_germany, xmin = 10.4005, ymin = 51.3371, xmax = 10.5152, ymax = 51.3856)
mapview(lucas_aoa, zcol = "class18", cex = 3, popup = F, layer.name = "LC")

lucas_aoa
lucas_aoa_ID <- lucas_aoa[,1]

saveRDS(lucas_aoa_ID, "./data/lucas/lucas_aoa_ID.rds")


