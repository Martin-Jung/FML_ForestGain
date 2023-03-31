library(terra)
library(sf)

# Set a seed for reproducability
set.seed(13002023)

p_ll <- '+proj=longlat +datum=WGS84 +no_defs'
e <- c(-180,180,-90,90)

# Total number of points
nr_points <- 1000 
# -------------------- #
## Load the respective layers ##

# Forest management categorizations. These show across all layers
# any grid cell that has forest gain in one of the 3 datasets and
# falls into a forest management class 
# 1 stating natural and 3 planted forest
fmlgain <- rast("extracts/combined_forestgain_2000onwards.tif")
names(fmlgain) <- "fml_gain"
fmlgain[fmlgain==0] <- NA

# Get also the Agreement layer. This layers indicates for each grid cell
# how many of the three datasets agree on whether there is forest gain
# Values rank from 1 to 3
dissim <- rast("resSaves/ForestGainAgreement_2000onwards.tif")
names(dissim) <- "Agreement"

# And the individual raster layers of total forest gain per grid cell
# Convert them to binary
ras1 <- rast("extracts/ESACCI_forestgainsum_2000onwards.tif")
ras1[ras1>0] <- 1
ras2 <- rast("extracts/Hansen_forestgain.tif")
ras2[ras2>0] <- 1
ras3 <- rast("extracts/modis_forestgainsum.tif")
ras3[ras3>0] <- 1

# Ensure that all layers perfectly align. They all have the same grid cell size,
# but are slightly different in extent at the borders (180deg dateline, here resample)
compareGeom(ras1,ras3)
ras2 <- terra::resample(ras2, ras1, method = "near")
compareGeom(ras1,ras2)
compareGeom(ras1,dissim)

fmlgain <- terra::resample(fmlgain, ras1, method = "near")
compareGeom(dissim,ras1)

# Add them all to a joint stack for easier sampling 
# and value extraction

# Make a combined version of any treecovergain
o <- sum(ras1, ras2, ras3, na.rm = TRUE)
o[o>0] <- 1; o[o==0] <- NA # Get any grid cell with tree-cover gain

ss <- c(fmlgain, dissim, o)

# Buffer the combined treecover gain dataset
# Load in and extract again
# Then do sampling
o <- rast("CombinedTreeCoverGain.tif")
bb <- rast("BoundaryForestGain.tif")
fmlgain <- resample(fmlgain, bb, method = "near")
bb <- mask(bb, fmlgain)
bb[bb==0] <- NA; bb[bb>0] <- 1

new_sampleboundary <- sum(bb, o, na.rm = TRUE)
new_sampleboundary[new_sampleboundary==0] <- NA

# --------------- #

# Now do a weighted random sample on the remaining layers
ex <- terra::spatSample(x = new_sampleboundary,
                        size = nr_points,
                        method = "random",
                        replace = FALSE, # No replacement
                        na.rm = TRUE, # No grid cells that fall into the ocean 
                        exhaustive = TRUE, # Try hard to reach the required number 
                        as.df = TRUE, # Return a data.frame
                        values = FALSE, # Also return cell values
                        cells = TRUE, # Also return cell numbers (needed for extent)
                        xy = TRUE, # Also return cell coordinates
                        warn = TRUE
                        )
# --- #

# Re-extract all relevant values
df <- terra::extract(c(ras1, ras2, ras3, dissim, bb), y = ex[,c("x", "y")])
ex <- cbind(ex, df)

ex$BoundaryForestGain[is.na(ex$BoundaryForestGain)] <- 0

write.csv(ex, "resSaves/validation_sample_random.csv", row.names = FALSE)

# Make checks of coverage
table(ex$fml_gain)
table(ex$Agreement)
table(ex$ESACCI_forestgainsum)
table(ex$Hansen_forestgain)
table(ex$modis_forestgainsum)

# --- #
ss <- c( rast("/mnt/idrive/jung/forMartina/ESACCI_forestgainsum.tif"),
         rast("/mnt/idrive/jung/forMartina/Hansen_forestgain.tif"),
         rast("/mnt/idrive/jung/forMartina/modis_forestgainsum.tif")
)
ex <- read.csv("resSaves/validation_sample_random.csv")
if(!hasName(ex, "X")) ex$X <- 1:nrow(ex)
template <- ss[[1]]
template[] <- NA

# Buffer each grid cell by a small amount
sub <- ex |> sf::st_as_sf(coords = c("x", "y"),crs = st_crs(ss))
sub$x <- st_coordinates(sub)[,1]
sub$y <- st_coordinates(sub)[,2]

template[terra::cellFromXY(template, ex[,c("x","y")])] <- ex$X
# Vectorize the specific grid cell
poly_rast <- as.polygons(template, values = TRUE, na.rm = TRUE)
names(poly_rast) <- "value"
poly_rast <- poly_rast |> sf::st_as_sf()

# Loop through each observation
for(i in 1:nrow(sub)){
  subs <- poly_rast |> dplyr::filter(value == i)

  sub[i,"xmin"] <- st_bbox(subs)["xmin"]
  sub[i,"ymin"] <- st_bbox(subs)["ymin"]
  sub[i,"xmax"] <- st_bbox(subs)["xmax"]
  sub[i,"ymax"] <- st_bbox(subs)["ymax"]
}
sub <- sub |> st_drop_geometry()
sub$cell <- NULL

# Save the outputs to a csv file
write.csv(sub, file = "resSaves/validation_sample_random.csv", row.names = FALSE)

# Also save a binary layer of the forestgain layers
# writeRaster(ras1, "resSaves/ESACCI_forestgainsum.tif", datatype = "INT1U", gdal=c("COMPRESS=DEFLATE"), overwrite = TRUE)
# writeRaster(ras2, "resSaves/Hansen_forestgain.tif", datatype = "INT1U", gdal=c("COMPRESS=DEFLATE"), overwrite = TRUE)
# writeRaster(ras3, "resSaves/modis_forestgainsum.tif", datatype = "INT1U",  gdal=c("COMPRESS=DEFLATE"), overwrite = TRUE)


