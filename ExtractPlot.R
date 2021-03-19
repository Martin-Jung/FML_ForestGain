library(raster)
library(gdalUtils)
library(ggplot2)
library(ggthemes)
library(data.table)
library(dplyr)
library(colorspace)
library(here)
here()
source('../../naturemap/src/000_ConvenienceFunctions.R')
rasterOptions(progress = 'text',tmpdir = 'E:/tmp/')
raster::removeTmpFiles(.1)
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

hansen_forestgain <- raster('extracts/Hansen_forestgain.tif')
modis_forestgain <- raster::stack('extracts/MODIS_forestgain.tif')
names(modis_forestgain) <- paste0('year',2001:2015)
esacci_forestgain <- raster::stack('extracts/ESACCI_forestgain.tif')
names(esacci_forestgain) <- paste0('year',1992:2015)


landarea <- raster('extracts/landarea.tif')
fml_natural <- raster('extracts/FML_natural.tif')
fml_aided <- raster('extracts/FML_aided.tif')
fml_planted <- raster('extracts/FML_planted.tif')
fml_consensus <- raster('extracts/consensus_forestgain.tif')
#fml_consensus <- alignRasters(fml_consensus, fml_natural, method = 'ngb', func = raster::modal,cl = TRUE)
#writeGeoTiff(fml_consensus, 'extracts/consensus_forestgain.tif')

assertthat::assert_that(
  compareRaster(fml_natural,fml_aided),
  compareRaster(fml_natural,fml_planted),
  compareRaster(fml_natural,fml_consensus),
  compareRaster(fml_natural,landarea)
)

# Now crop to time series
fml_natural <- raster::crop(fml_natural,hansen_forestgain)
fml_aided <- raster::crop(fml_aided,hansen_forestgain)
fml_planted <- raster::crop(fml_planted,hansen_forestgain)
fml_consensus <- raster::crop(fml_consensus,hansen_forestgain)

fml_natural[fml_natural==0] <- NA
fml_aided[fml_aided==0] <- NA
fml_planted[fml_planted==0] <- NA

assertthat::assert_that(
  compareRaster(fml_natural,hansen_forestgain),
  compareRaster(esacci_forestgain,hansen_forestgain),
  compareRaster(esacci_forestgain,modis_forestgain)
)

# -------- #
# Idea:
# Get statistics overall across datasets
# Plot stacked bargraph with proportion 

# TODO: Need to rerun with area estimates
ex <- bind_rows(
  # Hansen
  read.csv('extracts/zones_Hansen_naturalforest.csv') %>% 
    dplyr::mutate(type = 'natural', dataset = 'Hansen'),
  read.csv('extracts/zones_Hansen_plantedforest.csv') %>% 
    dplyr::mutate(type = 'planted', dataset = 'Hansen'),
 #  ESA CCI
 read.csv('extracts/zones_ESACCI_naturalforest.csv') %>% 
   dplyr::mutate(type = 'natural', dataset = 'ESACCI'),
 read.csv('extracts/zones_ESACCI_plantedforest.csv') %>% 
   dplyr::mutate(type = 'planted', dataset = 'ESACCI'),
 # MODIS
 read.csv('extracts/zones_MODIS_naturalforest.csv') %>% 
   dplyr::mutate(type = 'natural', dataset = 'MODIS'),
 read.csv('extracts/zones_MODIS_plantedforest.csv') %>% 
   dplyr::mutate(type = 'planted', dataset = 'MODIS') 
) %>% dplyr::filter(zone == 1)

# “replanted forest” - forest is managed and there
# are signs that the forest has been planted in the
# 100 m pixel. Rotation time is relatively long (>15 years).

# Short rotation plantations for timber
# Tree plantations: “woody plantations” - short rotation (15 years max) timber plantations.

ex$type <- factor(ex$type, levels = c('Natural regrowth', 'Replanted or'))

g <- ggplot(ex, aes(y = sum, x = dataset, group = type, fill = type)) +
  theme_classic(base_size = 18) +
  coord_flip() +
  geom_bar(stat = 'identity',colour='black', position = position_dodge(.5)) +
  scale_y_continuous(expand = c(0,0)) +
  scale_fill_brewer(palette = 'Dark2') +
  guides(fill = guide_legend(title = '')) +
  theme(legend.position = c(.9, .2),legend.text = element_text(size = 16))
g


# -------- #

# -------- #
# Second:
# Time series of forest gain per year split by whether it is natural regrowth, aided planted or commerical planted
# Show different lines per land cover dataset

# Area gain
esacci_area <- esacci_forestgain * landarea

df <- data.frame()
# Now simply mask and calculate cellStats
esacci_area_nat <- raster::mask(esacci_area, fml_natural,datatype = 'INT2S',options=c("COMPRESS=DEFLATE","PREDICTOR=2","ZLEVEL=9"))
df <- dplyr::bind_rows(
  data.frame(cellStats(esacci_area_nat,'sum'),dataset = 'ESACCI', type = 'natural' ),
  df
)
removeTmpFiles(1)
esacci_area_aid <- raster::mask(esacci_area, fml_aided,datatype = 'INT2S',options=c("COMPRESS=DEFLATE","PREDICTOR=2","ZLEVEL=9"))
df <- dplyr::bind_rows(
  data.frame(cellStats(esacci_area_aid,'sum'),dataset = 'ESACCI', type = 'aided' ),
  df
)
removeTmpFiles(1)
esacci_area_pla <- raster::mask(esacci_area, fml_planted,datatype = 'INT2S',options=c("COMPRESS=DEFLATE","PREDICTOR=2","ZLEVEL=9"))
df <- dplyr::bind_rows(
  data.frame(cellStats(esacci_area_pla,'sum'),dataset = 'ESACCI', type = 'planted' ),
  df
)

# Then MODIS
df <- dplyr::bind_rows(
  df,
  data.frame(fml_natural = values(fml_natural),
             fml_aided = values(fml_aided),
             fml_planted = values(fml_planted),
             values(modis_forestgain * landarea),
             dataset = 'MODIS')
)
# Finally Hansen separately



# -------- #
# Third:
# Map of hotspots of plantion planting
plot(fml_consensus)

# -------- #
# Combine all figures into one graph for publication


