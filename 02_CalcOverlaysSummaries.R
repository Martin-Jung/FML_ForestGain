# Format overlays and auxiliary datasets
library(terra)
library(stringr)
library(sf)
library(exactextractr)
library(assertthat)

# Load the aggregated forestgain layers from the various datasets
gain_modis <- rast("extracts/modis_forestgainsum.tif")
gain_hansen <- rast("extracts/Hansen_forestgain.tif")
# gain_esacci <- rast("extracts/ESACCI_forestgainsum.tif") # For ESA CCI, take only layers from 2000 onwards for consistency
esacci_forestgain <- rast('extracts/ESACCI_forestgain.tif')
names(esacci_forestgain) <- paste0('year',1992:2015)
esacci_forestgain <- subset(esacci_forestgain, paste0('year',2000:2015))
gain_esacci <- sum(esacci_forestgain, na.rm = TRUE)
names(gain_esacci) <- "gain_esacci"
landarea <- rast("extracts/landarea.tif")
  
# The remapped forest management layer
fml <- rast("extracts/FML_overallmode.tif")
fml_natural <- rast("extracts/FML_natural.tif")
fml_planted <- rast("extracts/FML_planted.tif")

# Resample all layers by nearest neighbour to the fml layer
gain_modis <- resample(gain_modis, fml, method = "near")
gain_hansen <- resample(gain_hansen, fml, method = "near")
gain_esacci <- resample(gain_esacci, fml, method = "near")

assert_that(compareGeom(gain_modis, gain_hansen, stopOnError=FALSE),
            compareGeom(gain_modis, fml, stopOnError=FALSE),
            compareGeom(gain_modis, gain_esacci, stopOnError=FALSE))

gains <- rast(list(gain_modis, gain_hansen, gain_esacci))

#### Country that pledged in BONN ----
# Extract for each country the total amount per dataset that is natural or managed

continents <- st_read("C:/Users/Martin/Downloads/ne_10m_admin_0_countries/JoinedContinents.shp")
countries <- st_read("C:/Users/Martin/Downloads/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp")

# Per class how much has there been regrown?
countries <- countries |> dplyr::select(ISO_A3, SOVEREIGNT, NAME_EN)

# Go through each layer and extract total sums of each gain estimate
out <- data.frame()
for(lyr in names(gains)){
  print(lyr)
  # Get and mask
  o <- gains[lyr]
  # Mask with fml_natural
  o1 <- mask(o, fml_natural, maskvalues = 0)
  o2 <- mask(o, fml_planted, maskvalues = 0)
  
  ex1 <- exactextractr::exact_extract(o1, countries, fun = "sum")
  ex2 <- exactextractr::exact_extract(o2, countries, fun = "sum")
  
  out <- rbind(out,
               data.frame(layer = lyr, type = "natural",
                          country_sov = countries$SOVEREIGNT, country = countries$NAME_EN, country_iso = countries$ISO_A3,
                          total_area_millha = (ex1/ 10000)/1e6 ),
               data.frame(layer = lyr, type = "planted",
                          country_sov = countries$SOVEREIGNT, country = countries$NAME_EN, country_iso = countries$ISO_A3,
                          total_area_millha = (ex2/ 10000)/1e6 )
               )
  rm(o1,o2,ex1,ex2)
}
# Save the output in ressaves
write.csv(out, "resSaves/OverlaySummary_countries.csv", row.names = FALSE)

#### Global biodiversity value from NatureMap ----
# Overlay overall gainsum with 10% and 30% NatureMap values
# Idea is to see whether managed forest gain primarily occurs 
# in areas considered to be of high value for biodiversity & carbon
if(!file.exists("resSaves/Overlay_naturemap10.tif")){
  nm <- rast("/mnt/hdrive/Projects/naturemap/results/10km_esh_ranked/minshort_speciestargets_biome.id__carbon__esh10km_repruns10_ranked.tif")
  
  # Get 10%
  nm_10 <- nm
  nm_10[nm_10>10] <- 0
  nm_10[nm_10>0] <- 1
  # project
  nm_10 <- project(nm_10, fml_natural, method = "near")
  terra::writeRaster(nm_10, "resSaves/Overlay_naturemap10.tif",overwrite=T)
  
  # Get 30%
  nm_30 <- nm
  nm_30[nm_30>30] <- 0
  nm_30[nm_30>0] <- 1
  # project
  nm_30 <- project(nm_30, fml_natural, method = "near")
  terra::writeRaster(nm_30, "resSaves/Overlay_naturemap30.tif",overwrite=T)
} else {
  # Load the overlays
  nm_10 <- rast("resSaves/Overlay_naturemap10.tif")
  nm_30 <- rast("resSaves/Overlay_naturemap30.tif")
  
  # Now for each zone, calculate the amount of forest per management
  out <- data.frame()
  for(lyr in names(gains)){
    print(lyr)
    # Get and mask
    o <- gains[lyr]
    # Mask with fml_natural
    o1 <- mask(o, fml_natural, maskvalues = 0)
    o2 <- mask(o, fml_planted, maskvalues = 0)
    # Mask with nm zone
    o1a <- mask(o1, nm_10, maskvalues = 0)
    o1b <- mask(o1, nm_30, maskvalues = 0)
    o2a <- mask(o2, nm_10, maskvalues = 0)
    o2b <- mask(o2, nm_30, maskvalues = 0)
    
    out <- rbind(out,
                 data.frame(
                   layer = lyr,
                   total_millha = (global(o, "sum",na.rm = TRUE)[[1]] / 10000)/1e6,
                   total_natural_millha = (global(o1, "sum",na.rm = TRUE)[[1]] / 10000)/1e6,
                   total_planted_millha = (global(o2, "sum",na.rm = TRUE)[[1]] / 10000)/1e6,
                   nm10_natural_millha = (global(o1a, "sum",na.rm = TRUE)[[1]] / 10000)/1e6,
                   nm30_natural_millha = (global(o1b, "sum",na.rm = TRUE)[[1]] / 10000)/1e6,
                   nm10_planted_millha = (global(o2a, "sum",na.rm = TRUE)[[1]] / 10000)/1e6,
                   nm30_planted_millha = (global(o2b, "sum",na.rm = TRUE)[[1]] / 10000)/1e6
                 ))
    rm(o, o1,o2,o1a,o1b,o2a,o2b)
  }  
  # Save the output in ressaves
  write.csv(out, "resSaves/OverlaySummary_naturemap.csv", row.names = FALSE)
}

#### Food production ----
# Overlay overall gainsum with foodscape intensity classes
# Here check howmuch tree cover gain occurred proportionally in areas
# under little, light and intense food production

if(!file.exists("resSaves/Overlay_foodintensity.tif")){
  # Get Foodscape intensity layer
  intensity <- rast("/mnt/hdrive/Otherstuff/TNC_Foodscapes/final_final/Foodscapes_combinedGEOTIFF_intensity.tif")
  
  # Align for resampling
  intensity <- resample(intensity, fml_natural, method = "near")
  
  # Save
  writeRaster(intensity, "resSaves/Overlay_foodintensity.tif",overwrite=T)
  
} else {
  # Load layer
  intensity <- rast("resSaves/Overlay_foodintensity.tif")
  
  # For intensity, we will calculate the overall zones per layer
  out <- data.frame()
  for(lyr in names(gains)){
    print(lyr)
    # Get and mask
    o <- gains[lyr]
    # Mask with fml_natural
    o1 <- mask(o, fml_natural, maskvalues = 0)
    o2 <- mask(o, fml_planted, maskvalues = 0)
    
    # Calculate zonal stats
    z1 <- zonal(o1, intensity, fun = "sum", na.rm = TRUE)
    names(z1) <- c("zone", "total_millha"); z1$total_millha <- (z1$total_millha / 10000)/1e6
    z1$layer <- lyr; z1$type <- "natural" 
    z2 <- zonal(o2, intensity, fun = "sum", na.rm = TRUE)
    names(z2) <- c("zone", "total_millha"); z2$total_millha <- (z2$total_millha / 10000)/1e6
    z2$layer <- lyr; z2$type <- "planted" 
    
    out <- rbind(out, rbind(z1,z2))
    rm(o, o1, o2)
  }  
  out$zone <- factor(out$zone, levels = c(1,2,3,101,102,103),
                     labels = c('Scattered cropland and grazing','Mixed and diverse food cultivation',
                                'Irrigated and/or intensive food production',
                                'Areas with little or only subsistence food production',
                                'Urbanized land', 'Inland water'))
  # Save the output in ressaves
  write.csv(out, "resSaves/OverlaySummary_foodintensity.csv", row.names = FALSE)
}

