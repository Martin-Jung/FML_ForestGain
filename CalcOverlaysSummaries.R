# Format overlays and auxilary datasets
library(terra)
library(stringr)
library(sf)
library(exactextractr)

# Load the aggregated forestgain layers


#### Country that pledged in BONN ----
# Extract for each country the total amount per dataset that is natural or managed

co <- st_read("/mnt/pdrive/bec_data/100_rawdata/GADM_AdminBoundaries/gadm_410.gpkg")

# Per class how much has there been regrown?
co$NAME_0

#### Global biodiversity value from NatureMap ----
# Overlay overall gainsum with 10% and 30% NatureMap values
# Idea is to see whether managed forest gain primarily occurs 
# in areas considered to be of high value for biodiversity & carbon
nm <- rast("/mnt/hdrive/Projects/naturemap/results/10km_esh_ranked/minshort_speciestargets_biome.id__carbon__esh10km_repruns10_ranked.tif")

# Get 10%
nm_10 <- nm
nm_10[nm_10>10] <- 0
nm_10[nm_10>0] <- 1
terra::writeRaster(nm_10, "resSaves/Overlay_naturemap10.tif")

# Get 30%
nm_30 <- nm
nm_30[nm_30>30] <- 0
nm_30[nm_30>0] <- 1
terra::writeRaster(nm_30, "resSaves/Overlay_naturemap30.tif")

#### Food security ----
# Overlay overall gainsum with foodscape intensity classes
# Here check howmuch tree cover gain occurred proportionally in areas
# under little, light and intense food production

# Get Foodscape intensity layer
intensity <- rast("/mnt/hdrive/Otherstuff/TNC_Foodscapes/final_final/Foodscapes_combinedGEOTIFF_intensity.tif")

writeRaster(intensity, "resSaves/Overlay_foodintensity.tif")

#### Protected areas ----
# Could also extract inside/outside protected areas?
