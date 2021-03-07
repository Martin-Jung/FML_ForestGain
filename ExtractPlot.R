library(raster)
library(gdalUtils)
library(ggplot2)
library(ggthemes)
library(colorspace)
library(here)
here()
source('../../naturemap/src/000_ConvenienceFunctions.R')
rasterOptions(progress = 'text')
# --- #
p_ll <- '+proj=longlat +datum=WGS84 +no_defs'
e <- c(-180,180,-90,90)

# ---- #
#### Format extracts
# Natural as template
fml_natural <- raster('extracts/FML_natural.tif')

# Load landarea and build
ll <- list.files('extracts/','landarea',full.names = TRUE)
gdalbuildvrt(ll,output.vrt = 'extracts/out.vrt',
                         resolution = 'highest',separate = FALSE)
ras <- raster('extracts/out.vrt')
proj4string(ras) <- p_ll
writeGeoTiff(ras,'extracts/landarea.tif',dt = 'INT4U')
ras <- raster('extracts/landarea.tif')
ras <- raster::crop(ras, e )
# Align
ras <- alignRasters(ras, fml_natural,method = 'bilinear',func = mean,cl = TRUE)
writeGeoTiff(ras,'extracts/landarea.tif',dt = 'INT4U')
file.remove('extracts/out.vrt')

# Load ESACCI and build
ll <- list.files('extracts/','ESACCI_forestgain',full.names = TRUE)
gdalbuildvrt(ll, output.vrt = 'extracts/out.vrt',
             resolution = 'highest',separate = FALSE)
gdal_translate(src_dataset = 'extracts/out.vrt',
               dst_dataset = 'extracts/ESACCI_forestgain.tif',
               r = 'nearest',
               co = c("COMPRESS=DEFLATE","PREDICTOR=2","ZLEVEL=9"),
               ot = 'Byte')
ras <- raster::stack('extracts/ESACCI_forestgain.tif')
names(ras) <- paste0('year',1992:2015)
proj4string(ras) <- p_ll
ras <- raster::crop(ras, e )
# Align
gdalwarp(srcfile = 'extracts/MODIS_forestgain.tif',
         dstfile = 'extracts/MODIS_forestgain2.tif',
         co = c("COMPRESS=DEFLATE","PREDICTOR=2","ZLEVEL=9"),
         te = c(xmin(fml_natural),ymin(fml_natural),xmax(fml_natural),ymax(fml_natural)),
         tr = res(fml_natural),
         r = 'near',
         multi = TRUE
         )
writeGeoTiff(ras,'extracts/ESACCI_forestgain.tif',dt = 'INT1U')
file.remove('extracts/out.vrt')

# Load MODIS forestgain
ll <- list.files('extracts/','MODIS_forestgain',full.names = TRUE)
gdalbuildvrt(ll, output.vrt = 'extracts/out.vrt',
             resolution = 'highest',separate = FALSE)
gdal_translate(src_dataset = 'extracts/out.vrt',
               dst_dataset = 'extracts/MODIS_forestgain.tif',
               r = 'nearest',
               co = c("COMPRESS=DEFLATE","PREDICTOR=2","ZLEVEL=9"),
               ot = 'Byte')
ras <- raster::stack('extracts/MODIS_forestgain.tif')
names(ras) <- paste0('year',2001:2015)
proj4string(ras) <- p_ll
ras <- raster::crop(ras, e )
writeGeoTiff(ras,'extracts/MODIS_forestgain2.tif',dt = 'INT1U')
file.remove('extracts/out.vrt')

# Hansen Forest gain
ras <- raster('extracts/Hansen_forestgain.tif')
proj4string(ras) <- p_ll
ras <- raster::crop(ras, e )
writeGeoTiff(ras,'extracts/Hansen_forestgain.tif',dt = 'INT1U')


#### Extract data ####
# First load and extract

fml_natural <- raster('extracts/FML_natural.tif')
fml_planted <- raster('extracts/FML_planted.tif')
landarea <- raster('extracts/landarea.tif')
hansen_forestgain <- raster('extracts/Hansen_forestgain.tif')
modis_forestgain <- raster::stack('extracts/MODIS_forestgain.tif')
esacci_forestgain <- raster::stack('extracts/ESACCI_forestgain.tif')

df <- data.frame(landarea = values(landarea),
                 fml_natural = values(fml_natural),
                 fml_planted = values(fml_planted)
                 ) %>% 
  tidyr::drop_na()

# -------- #
# Idea:
# Get statistics overall across datasets
# Plot stacked bargraph with proportion 


# -------- #
# Second:
# Time series of forest gain per year split by whether it is natural regrowth or planted
# Show different lines per land cover dataset


# -------- #
# Third:
# Map of hotspots of plantion planting

# -------- #
# Combine all figures into one graph for publication


