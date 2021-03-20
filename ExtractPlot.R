library(raster)
library(gdalUtils)
library(ggplot2)
library(ggthemes)
library(data.table)
library(dplyr)
library(tmap)
library(scales)
library(colorspace)
library(here)
here()
source('../../naturemap/src/000_ConvenienceFunctions.R')
rasterOptions(progress = 'text',
              tmpdir = 'E:/tmp/'
              )
raster::removeTmpFiles(.1)
# --- #
p_ll <- '+proj=longlat +datum=WGS84 +no_defs'
e <- c(-180,180,-90,90)

cols <- c("Unmanaged regrowth" = "#3B443C",
          "Aided replanting" = '#6ECCBB',
          "Plantations" = '#D2081D')

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

ex <- bind_rows(
  # Hansen
  read.csv('extracts/hansen_natural.csv') %>% rename(remapped = gain) %>% 
    dplyr::mutate(type = 'natural', dataset = 'Hansen'),
  read.csv('extracts/hansen_aided.csv') %>% rename(remapped = gain) %>% 
    dplyr::mutate(type = 'aided', dataset = 'Hansen'),
  read.csv('extracts/hansen_planted.csv') %>% rename(remapped = gain) %>% 
    dplyr::mutate(type = 'planted', dataset = 'Hansen'),
 #  ESA CCI
 read.csv('extracts/esacci_natural.csv') %>%
   dplyr::mutate(type = 'natural', dataset = 'ESACCI'),
 read.csv('extracts/esacci_aided.csv') %>%
   dplyr::mutate(type = 'aided', dataset = 'ESACCI'),
 read.csv('extracts/esacci_planted.csv') %>%
   dplyr::mutate(type = 'planted', dataset = 'ESACCI'),
 # MODIS
 read.csv('extracts/modis_natural.csv') %>%
   dplyr::mutate(type = 'natural', dataset = 'MODIS'),
 read.csv('extracts/modis_aided.csv') %>%
   dplyr::mutate(type = 'aided', dataset = 'MODIS'),
 read.csv('extracts/modis_planted.csv') %>%
   dplyr::mutate(type = 'planted', dataset = 'MODIS')
)

# Convert to million ha
ex$remapped <- (ex$remapped / 1e6) / 0.0001

# “replanted forest” - forest is managed and there
# are signs that the forest has been planted in the
# 100 m pixel. Rotation time is relatively long (>15 years).

# Short rotation plantations for timber
# Tree plantations: “woody plantations” - short rotation (15 years max) timber plantations.

ex$type <- factor(ex$type, levels = c('natural','aided','planted'), labels = names(cols))

scientific_10 <- function(x) {
  parse(text=gsub("e", " %*% 10^", scales::scientific_format()(x)))
}

g <- ggplot(ex, aes(y = remapped, x = dataset, group = type, fill = type)) +
  theme_classic(base_size = 18) +
  coord_flip() +
  geom_bar(stat = 'identity',colour='black', position = position_dodge(.5)) +
  scale_y_continuous(expand = c(0,0),breaks = pretty_breaks(5), labels = scientific_10 ) +
  scale_fill_manual(values = cols) +
  guides(fill = guide_legend(title = '')) +
  theme(legend.position = c(.75, .35),legend.text = element_text(size = 16),legend.background = element_blank()) +
  labs(x = '', y = 'Forest regrowth (in mill. ha)')
g
# Add mean estimate above the plot
gs <- ggplot(ex, aes(y = remapped, x = type, colour = type)) +
  theme_void() +
  # theme_classic(base_size = 18) +
  coord_flip() +
  stat_summary(fun = mean,
               fun.min = function(x) mean(x) - sd(x), 
               fun.max = function(x) mean(x) + sd(x), 
               geom = "pointrange",size = 1.5) +
  scale_colour_manual(values = cols) + 
  scale_x_discrete( expand=c(1, 1) ) +
  theme(axis.line.y = element_blank(),axis.text.y = element_blank(),axis.ticks.y = element_blank()) +
  guides(colour = 'none')
gs  

# gg <- cowplot::plot_grid(gs + theme(plot.margin = grid::unit(c(0, 0, 0, 0), "cm")),
#                    g ,#+ theme(plot.margin = grid::unit(c(0, 0, 0, 0), "cm")),
#                    rel_heights = c(1,3),
#                    align = 'hv',scale = TRUE, ncol = 1)
# gg
# cowplot::ggsave2(plot = gg,filename = 'Figure_bar.png',width = 10,height = 7,dpi = 400)

# Alternative.
# Annotate as grob
g + annotation_custom(grob = ggplotGrob(gs), xmin = 3.3, xmax = 3.6, ymin = 0, ymax = Inf)

ggsave(plot = g + annotation_custom(grob = ggplotGrob(gs), xmin = 3.3, xmax = 3.6, ymin = 0, ymax = Inf),
       filename = 'Figure_bar.png',width = 10,height = 6,dpi = 400 )

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
fml_consensus <- raster('extracts/consensus_forestgain.tif')
NAvalue(fml_consensus) <- 0
# Max aggregate it for plotting
fml_consensus <- raster::aggregate(fml_consensus, fact = 10, fun = raster::modal, na.rm = TRUE)
# fml_consensus <- raster::focal(fml_consensus, w = matrix(1/25,nrow=5,ncol=5),
#                                fun = function(x) max(x, na.rm = TRUE))

# Tmap data
library(sf)
data(World)
World <- World %>% dplyr::filter(continent != 'Antarctica')

fml_consensus <- ratify(fml_consensus)
rat <- levels(fml_consensus)[[1]]
rat$cat <- names(cols)
rat$ID <- c(1,2,3)
levels(fml_consensus) <- rat

tm <- tm_shape(fml_consensus, bbox = st_bbox(fml_consensus), projection = "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs") +
  tm_raster("r_tmp_2021-03-20_222047_9896_58218.grd",  style = 'cat', labels = names(cols),
            palette = cols, title = "Forest regrowth") +
  tm_shape(World) +
  tm_borders(col = "grey20",lwd = .1) +
  tm_layout(scale = .65, 
            legend.position = c("left","bottom"),
            legend.bg.color = "white", legend.bg.alpha = .2, 
            legend.frame = "gray50")

tmap_save(tm,filename = 'Figure_map.png',width = 1400,height = 900,dpi = 400)
# -------- #
# Combine all figures into one graph for publication


