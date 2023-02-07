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
fmlgain <- rast("extracts/combined_forestgain.tif")
names(fmlgain) <- "fml_gain"

# Get also the Agreement layer. This layers indicates for each grid cell
# how many of the three datasets agree on whether there is forest gain
# Values rank from 1 to 3
dissim <- rast("resSaves/ForestGainAgreement.tif")
names(dissim) <- "Agreement"

# And the individual raster layers of total forest gain per grid cell
# Convert them to binary
ras1 <- rast("extracts/ESACCI_forestgainsum.tif")
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
ss <- c(fmlgain, dissim, ras1, ras2, ras3)

#### Random sample ####
# Now do a weighted random sample
ex <- terra::spatSample(x = ss,
                        size = nr_points,
                        method = "random",
                        replace = FALSE, # No replacement
                        na.rm = TRUE, # No grid cells that fall into the ocean 
                        exhaustive = TRUE, # Try hard to reach the required number 
                        as.df = TRUE, # Return a data.frame
                        values = TRUE, # Also return cell values
                        cells = TRUE, # Also return cell numbers (needed for extent)
                        xy = TRUE, # Also return cell coordinates
                        warn = TRUE
                        )
write.csv(ex, "resSaves/validation_sample_random.csv", row.names = FALSE)

# Make checks of coverage
table(ex$fml_gain)
table(ex$Agreement)
table(ex$ESACCI_forestgainsum)
table(ex$Hansen_forestgain)
table(ex$modis_forestgainsum)

# Get only those cells with values globally
sub <- ras1
sub[] <- NA
sub[ex$cell] <- ex$cell

sub <- rast(ncols=10, nrows=10)
values(sub) <- 1:ncell(sub)

# Vectorize by grid cell
subs <- terra::as.polygons(x = sub, trunc = FALSE,
                           dissolve = FALSE, values = TRUE,
                           na.rm = TRUE)

# Loop over and extract border coordinates for each (ymin, ymax, xmin, xmax)
pb <- progress::progress_bar$new(total = length(nrow(subs)))
for(i in 1:nrow(subs)){
  pb$tick()
  s <- subs[i]
  ex[which(ex$cell == s[[1]]), "xmin"] <- st_bbox(s)["xmin"]
  ex[which(ex$cell == s[[1]]), "xmax"] <- st_bbox(s)["xmax"]
  ex[which(ex$cell == s[[1]]), "ymax"] <- st_bbox(s)["ymax"]
  ex[which(ex$cell == s[[1]]), "ymin"] <- st_bbox(s)["ymin"]
  rm(s)
}
rm(pb)

# Save the outputs to a csv file
ex$cell <- NULL
write.csv(ex, file = "resSaves/validation_sample_random.csv", row.names = TRUE)
