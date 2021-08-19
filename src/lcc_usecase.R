# Goal is a landcover classification covering all of germany, 
# including a random forest model training and prediction

# load package
library(openeo)
library(sf)

# establish connection
con <- connect(host = "https://openeo.vito.be")

# authenticate
login(login_type="basic",
      user="",
      password="")

# get a process graph builder, see ?processes
p <- processes()

# bigger, for LUCAS testing
aoa <- list(west = 10.4005, south = 51.3371, east = 10.5152, north = 51.3856) # niederorschel
# smaller, for faster runs
aoa <- list(west = 10.452617, south = 51.361166, east = 10.459773, north = 51.364194)
t <- c("2018-07-01", "2018-10-01")

cube_s2 <- p$load_collection(
  id = "SENTINEL2_L2A_SENTINELHUB",
  spatial_extent = aoa,
  temporal_extent = t,
  # load less bands for faster computation
  bands=c("B02", "B03", "B04", "B05", "B06", "B07", "B08", "B8A", "B11", "B12")
  # bands= c("B02", "B04", "B08")
  # AFAIK resolution stays at highest by default
)

# load SCL in separate collection, must be same aoi and t extent!
cube_SCL <- p$load_collection(
  id = "SENTINEL2_L2A_SENTINELHUB",
  spatial_extent = aoa,
  temporal_extent = t,
  bands=c("SCL")
)

# for efficiency, do filter_spatial here
# read lucas data from file (contains SF object), TODO do extraction here!
lucas_aoa <- readRDS("./data/lucas/lucas_aoa_ID.rds")
# buffer, because only polygons allowed
lucas_aoa_buf <- st_buffer(lucas_aoa, 10)
# library(mapview)
# mapview(lucas_aoa_buf[1,])
# extract at polygon location
cube_s2 <- p$filter_spatial(data = cube_s2, geometries = lucas_aoa_buf)
cube_SCL <- p$filter_spatial(data = cube_SCL, geometries = lucas_aoa_buf)

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

# compute indices here, possibly move up when needed per season or something
# CHANGE BAND INDICES HERE WHEN CHANGING BANDS LOADED
# ("B02", "B03", "B04", "B05", "B06", "B07", "B08", "B8A", "B11", "B12")

ndvi_ <- function(x, context) {
  b4 <- x[3]
  b8 <- x[7]
  return(p$normalized_difference(b8, b4))
}

evi_ <- function(x, context) {
  b2 <- x[1]
  b4 <- x[3]
  b8 <- x[7]
  return((2.5 * (b8 - b4)) / ((b8 + 6 * b4 - 7.5 * b2) + 1))
}

cube_s2_yearly_ndvi <- p$reduce_dimension(cube_s2_yearly_composite, ndvi_, "bands")
cube_s2_yearly_ndvi <- p$add_dimension(cube_s2_yearly_ndvi, name = "bands", label = "NDVI", type = "bands")

cube_s2_yearly_evi <- p$reduce_dimension(cube_s2_yearly_composite, evi_, "bands")
cube_s2_yearly_evi <- p$add_dimension(cube_s2_yearly_evi, name = "bands", label = "EVI", type = "bands")

# merge cubes
cube_s2_yearly_merge1 <- p$merge_cubes(cube_s2_yearly_composite, cube_s2_yearly_ndvi)
cube_s2_yearly_merge2 <- p$merge_cubes(cube_s2_yearly_merge1, cube_s2_yearly_evi)

## AGGREGATION OF LUCAS DATA ####################
## ISSUES:
## no point, only polygon support on VITO
## aggregate_spatial only works with a temporal dimension present
## atttributes are not preserved in aggregation

# read lucas data from file (contains SF object), TODO do extraction here!
# lucas_aoa <- readRDS("./data/lucas/lucas_aoa_ID.rds")
# cube_s2_yearly_extr <- p$filter_spatial(data = cube_s2_yearly_merge2, geometries = lucas_aoa_buf)

# aggregate geometries, reducer is not needed but passed anyway
# cube_s2_yearly_agg <- p$aggregate_spatial(data = cube_s2_yearly_composite, reducer = p$mean, geometries = lucas_aoa)

# create result node
res <- p$save_result(data = cube_s2_yearly_merge2, format = "NetCDF", options = list(sample_by_feature = TRUE))

# export with option
# res <- p$save_result(data = cube_s2_yearly_extr, format = "NetCDF", options = list(sample_by_feature = TRUE))

# send job to back-end
job <- create_job(graph = res, title = "test extract batch 10m")

start_job(job = job)
