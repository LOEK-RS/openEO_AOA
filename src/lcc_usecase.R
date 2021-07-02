# Goal is a landcover classivication covering all of germany, 
# including a random forest model training and prediction

# Issues, TODO
# in smaller timespans, filter errors are visible (e.g. cloud shadows)

# load package
library(openeo)

# establish connection
con <- connect(host = "https://openeo.vito.be")

# authenticate
login(login_type="basic",
      user="",
      password="")

# get a process graph builder, see ?processes
p <- processes()

aoa <- list(west = 10.4005, south = 51.3371, east = 10.5152, north = 51.3856) # niederorschel
t <- c("2018-01-01", "2018-03-01")

cube_s2 <- p$load_collection(
  id = "SENTINEL2_L2A_SENTINELHUB",
  spatial_extent = aoa,
  temporal_extent = t,
  # bands=c("B02", "B03", "B04", "B05", "B06", "B07", "B08", "B8A", "B11", "B12")
  bands= c("B04", "B03", "B02")
  # AFAIK resolution stays the highest by default
)

# load SCL in separate collection, must be same aoi and t extent!
cube_SCL <- p$load_collection(
  id = "SENTINEL2_L2A_SENTINELHUB",
  spatial_extent = aoa,
  temporal_extent = t,
  bands=c("SCL")
)

# NDVI, NBR, EVI

# merge cubes accordingly (all into bands dimension)

# define filter function to create mask from a cube that only contains 1 band: SCL
clouds_ <- function(data, context) {
  SCL <- data[1] # select SCL band
  # we wanna keep:
  veg <- p$eq(SCL, 4) # select pixels with the respective codes
  no_veg <- p$eq(SCL, 5)
  water <- p$eq(SCL, 6)
  unclassified <- p$eq(SCL, 7)
  snow <- p$eq(SCL, 11)
  # or has only 2 arguments so..
  or1 <- p$or(veg, no_veg)
  or2 <- p$or(water, unclassified)
  or3 <- p$or(or2, snow)
  # create mask
  return(p$not(p$or(or1, or3)))
}

# create mask by reducing bands with our defined formula
cube_SCL_mask <- p$reduce_dimension(data = cube_SCL, reducer = clouds_, dimension = "bands")

# mask the S2 cube
cube_s2_masked <- p$mask(cube_s2, cube_SCL_mask) 
# default: replaced with 0. change to -99?

cube_s2_yearly_composite <- p$reduce_dimension(cube_s2_masked, function(x, context) {
  p$median(x, ignore_nodata = TRUE)
}, "t")

# create result node
res <- p$save_result(data = cube_s2_yearly_composite, format = "GTiff")

# send job to back-end
job <- create_job(graph = res, title = "composite_test")

