library(raster)
library(gdalUtils)
library(ggplot2)
library(ggthemes)
library(data.table)
library(dplyr)
library(forcats)
library(tmap)
library(stars)
library(scales)
rasterOptions(progress = 'text')
raster::removeTmpFiles(.1)
# --- #
p_ll <- '+proj=longlat +datum=WGS84 +no_defs'
e <- c(-180,180,-90,90)

cols <- c("Natural forest" = "#2A9E00",
          "Managed forest" = '#943B60')

# ---- #
# -------- #
#### Figure - part overall (old) ####
# Idea:
# Get statistics overall across datasets
# Plot stacked bargraph with proportion 

ex <- bind_rows(
  # Hansen
  read.csv('extracts/hansen_natural.csv') %>% rename(remapped = gain) %>% 
    dplyr::mutate(type = 'natural', dataset = 'Hansen'),
  read.csv('extracts/hansen_planted.csv') %>% rename(remapped = gain) %>% 
    dplyr::mutate(type = 'planted', dataset = 'Hansen'),
 #  ESA CCI
 read.csv('extracts/esacci_natural.csv') %>%
   dplyr::mutate(type = 'natural', dataset = 'ESACCI'),
 read.csv('extracts/esacci_planted.csv') %>%
   dplyr::mutate(type = 'planted', dataset = 'ESACCI'),
 # MODIS
 read.csv('extracts/modis_natural.csv') %>%
   dplyr::mutate(type = 'natural', dataset = 'MODIS'),
 read.csv('extracts/modis_planted.csv') %>%
   dplyr::mutate(type = 'planted', dataset = 'MODIS')
)

# Convert to million ha
ex$remapped <- (ex$remapped * (0.0001)) /1e6

# “replanted forest” - forest is managed and there
# are signs that the forest has been planted in the
# 100 m pixel. Rotation time is relatively long (>15 years).

# Short rotation plantations for timber
# Tree plantations: “woody plantations” - short rotation (15 years max) timber plantations.

ex$type <- factor(ex$type, levels = c('natural','planted'), labels = names(cols))

scientific_10 <- function(x) {
  parse(text=gsub("e", " %*% 10^", scales::scientific_format()(x)))
}

# Some stats
ex <- left_join(ex, ex %>% dplyr::group_by(dataset) %>% summarise(tot = sum(remapped)) )  # Total per dataset
ex %>% group_by(type) %>% 
  summarise(prop_avg = mean(remapped / tot),
            sd_avg = sd(remapped / tot),
            min = min( remapped / tot),
            max = max( remapped / tot)
            )

g <- ggplot(ex, aes(y = remapped, x = dataset, group = type, fill = type)) +
  theme_light(base_size = 18) +
  coord_flip() +
  geom_bar(stat = 'identity',colour='black', position = position_dodge(.5)) +
  # scale_y_continuous(expand = c(0,0),breaks = pretty_breaks(5)) + #, labels = scientific_10 ) +
  scale_fill_manual(values = cols) +
  guides(fill = guide_legend(title = '')) +
  theme(legend.position = c(.75, .35),legend.text = element_text(size = 16),legend.background = element_blank()) +
  labs(x = '', y = 'Treecover gain (in mill. ha)')
g
ggsave(filename = "figures/SIFigure1_overalltreecovergain.png",plot = g)

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

gg <- cowplot::plot_grid(gs + theme(plot.margin = grid::unit(c(0, 0, 0, 0), "cm")),
                   g ,#+ theme(plot.margin = grid::unit(c(0, 0, 0, 0), "cm")),
                   rel_heights = c(1,3),
                   align = 'hv',scale = TRUE, ncol = 1)
gg
# Alternative.
# Annotate as grob
g1 <- g + annotation_custom(grob = ggplotGrob(gs), xmin = 3.3, xmax = 3.6, ymin = 0, ymax = Inf)

ggsave(plot = g + annotation_custom(grob = ggplotGrob(gs), xmin = 3.3, xmax = 3.6, ymin = 0, ymax = Inf),
       filename = "figures/SIFigure1_overalltreecovergain.png",width = 10,height = 6,dpi = 400 )

# -------- #
#### Figure 1 - time series ####
# Second:
# Time series of forest gain per year split by whether it is natural regrowth, aided planted or commerical planted
# Show different lines per land cover dataset
# Calculated and extracted via earth engine

# Get extracted stats
stats1 <- read.csv('extracts/stats_esacci_natural.csv') %>% dplyr::select(contains('remapped'))
stats1 <- sort(stats1);names(stats1) <- paste0('year', 1992:2015);stats1$type <- 'natural';stats1$dataset <- 'esacci'
stats3 <- read.csv('extracts/stats_esacci_planted.csv') %>% dplyr::select(contains('remapped'))
stats3 <- sort(stats3);names(stats3) <- paste0('year', 1992:2015);stats3$type <- 'planted';stats3$dataset <- 'esacci'

stats4 <- read.csv('extracts/stats_modis_natural.csv') %>% dplyr::select(contains('remapped'))
stats4 <- sort(stats4);names(stats4) <- paste0('year', 2001:2015);stats4$type <- 'natural';stats4$dataset <- 'modis'
stats6 <- read.csv('extracts/stats_modis_planted.csv') %>% dplyr::select(contains('remapped'))
stats6 <- sort(stats6);names(stats6) <- paste0('year', 2001:2015);stats6$type <- 'planted';stats6$dataset <- 'modis'

# And hansen
stats7 <- read.csv('extracts/hansen_natural.csv') %>% dplyr::select(contains('gain'))
stats7 <- cbind(stats7, replicate(12,stats7))
names(stats7) <- paste0('year', 2000:2012);stats7$type <- 'natural';stats7$dataset <- 'hansen'
stats9 <- read.csv('extracts/hansen_planted.csv') %>% dplyr::select(contains('gain'))
stats9 <- cbind(stats9, replicate(12,stats9))
names(stats9) <- paste0('year', 2000:2012);stats9$type <- 'planted';stats9$dataset <- 'hansen'

ts_stats <- bind_rows(
  stats1,stats3,
  stats4,stats6,
  stats7,stats9
) %>% 
  tidyr::pivot_longer(cols = contains('year')) %>% 
  dplyr::mutate(year = as.numeric(gsub('\\D','', name)) ) %>% 
  # Convert to millha
  dplyr::mutate(millha = (value * 0.0001)/1e6 )

ts_stats$type <- factor(ts_stats$type,
                        levels = c('natural','planted'),
                        labels = names(cols)
                        )
ts_stats$dataset <- factor(ts_stats$dataset,
                           levels = c('hansen','modis','esacci'),
                           labels = c('Hansen','MODIS','ESA CCI')
                             )

dd <- ts_stats %>% dplyr::filter(dataset %in% c('ESA CCI','MODIS'))
ddh <- ts_stats %>% dplyr::filter(dataset %in% c('Hansen'))

# Calculate since year 2000
gs <- ggplot(ts_stats %>% dplyr::filter(year>=2000), aes(y = millha, x = type, colour = type)) +
  theme_void(base_size = 18) +
  stat_summary(fun = mean,
               fun.min = function(x) mean(x) - sd(x), 
               fun.max = function(x) mean(x) + sd(x), 
               geom = "pointrange",size = 1.5) +
  scale_colour_manual(values = cols) + 
  scale_x_discrete( expand=c(1, 1) ) +
  theme(axis.line.y = element_blank(),axis.text.y = element_blank(),axis.ticks.y = element_blank()) +
  guides(colour = 'none')
gs  

# Build time series plot
g2 <- ggplot(dd,
            aes(x = year, y = millha,
                          colour = type, linetype = dataset)) +
  theme_classic(base_size = 18) +
  geom_line( size = 1.5) +
  # Add Hansen as alpha line with point at the end
  geom_line(data = ddh,  alpha = .4, size = 2,show.legend = FALSE) + 
    # geom_point(data = ddh %>% dplyr::filter(year == 2012), size = 4,show.legend = FALSE) +
  # annotate(geom = 'rect', y = ) +
  scale_colour_manual(values = cols) + 
  scale_linetype_stata() +
  scale_y_continuous(breaks = pretty_breaks(5) ) +
  # Add abline
  geom_vline(xintercept = 2000) +
  annotation_custom(grob = ggplotGrob(gs), xmin = 2015, xmax = 2016.5, ymin = 0, ymax = Inf) +
  guides(colour = 'none', alpha = 'none',
         linetype = guide_legend(title = '',ncol = 1,keywidth = unit(1,'in'),label.position = 'bottom')) +
  theme(legend.position = c(.1,.75),legend.text = element_text(size = 16),legend.background = element_blank()) +
  labs(x = '', y = 'Treecover gain (in mill. ha)')
g2
ggsave(plot = g2,filename = 'figures/Figure_ts.png',width = 12,height = 6,dpi = 400)

# -------- #
#### Figure 1 - Map ####
# Map of hotspots of plantation planting
if(!file.exists('extracts/consensus_forestgain_aggMode.tif')){
  fml_consensus <- raster('extracts/combined_forestgain.tif')
  NAvalue(fml_consensus) <- 0
  # Max aggregate it for plotting
  fml_consensus <- raster::aggregate(fml_consensus, fact = 10, fun = max, na.rm = TRUE) # Changed to max, assuming larger management being more predominent
  ibis.iSDM:::writeGeoTiff(fml_consensus, 'extracts/consensus_forestgain_aggMode.tif',dt = 'INT2S')
}
fml_consensus <- raster('extracts/consensus_forestgain_aggMode.tif')
# fml_consensus <- raster::focal(fml_consensus, w = matrix(1/25,nrow=5,ncol=5),
#                                fun = function(x) modal(x, na.rm = TRUE))

# Tmap data
library(sf)
data(World)
World <- World %>% dplyr::filter(continent != 'Antarctica')

moll = "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
# fml_consensus <- projectRaster(fml_consensus,crs = moll,method = "ngb")

fml_consensus <- ratify(fml_consensus)
rat <- levels(fml_consensus)[[1]]
rat$cat <- names(cols)
rat$cat[1] <- "Naturally regenerating\nforest"
rat$ID <- c(1,3)
levels(fml_consensus) <- rat

tm <- tm_shape(World,is.master = FALSE, projection = moll) +
  tm_style(style = 'natural') +
    tm_borders(col = "grey20",lwd = .1) + tm_fill(col = 'white',zindex = 1) +
  tm_shape(fml_consensus, projection = moll) +
    tm_raster("layer",  style = 'cat', labels = names(cols),
              palette = cols, title = "",legend.hist = FALSE) +
  tm_layout(scale = .65,
           # main.title = "Tree cover gain until 2015",main.title.position = 'center',main.title.size = .75,
            earth.boundary = TRUE,earth.boundary.lwd = 1,
            legend.position = c(.4,.15),#c("center",'bottom'),
            legend.bg.color = "white", legend.bg.alpha = 0, legend.text.size = .6,
            legend.frame = FALSE, #legend.frame = "gray50"
            frame = FALSE, inner.margins = 0, outer.margins = 0, asp = 0, legend.width = 400
            ) 
# tm_legend(legend.outside=T, legend.outside.position="right")
tmap_save(tm,filename = 'figures/Figure_map.png',width = 1400,height = 900,dpi = 400)

#### Figure 1 - Prop coverage ####
# Using the extracted values from biodiversity, countries with pledges and food,
# simply highlight where each dataset lies in terms of its estimate

# Countries
data("World")
ex <- read.csv('resSaves/OverlaySummary_countries.csv')
# Join in Bonn pledges
pledge <- readxl::read_xlsx('pledges.xlsx')
# Manual reformating
pledge$country[pledge$country=='Scotland'] <- 'United Kingdom'
# pledge$country[pledge$country=='United States of America'] <- 'United States'
pledge$country[pledge$country=="C\u00f4te d'Ivoire"] <- "Ivory Coast"
pledge$country[pledge$country=="Republic of Sudan"] <- "Sudan"
pledge$country[pledge$country=="Tanzania"] <- "United Republic of Tanzania"
pledge$country[pledge$country=="Eswatini"] <- "eSwatini"

assertthat::assert_that(
  length(pledge$country[which(!(pledge$country %in% ex$country_sov))]) == 0
)
# Join in 
ex <- dplyr::left_join(ex |> dplyr::select(-country), pledge, by = c('country_sov' = 'country'))
# Add income groups
# ex <- dplyr::left_join(ex, World |> dplyr::select(income_grp,sovereignt) |> sf::st_drop_geometry(),
#                        by = c('country_sov' = 'sovereignt'))

ex$layer <- factor(ex$layer, levels = c("modis_forestgainsum", "gain_esacci", "Hansen_forestgain"),
                   labels = c("MODIS", "ESA CCI", "Hansen"))
ex$type <- factor(ex$type, levels = c("natural", "planted"), labels = names(cols))

# Figure idea:
# Calculate the amount of tree cover gain for countries and without pledges as relative barplot
ex2 <- ex |> filter(!is.na(pledge_ha)) |> 
  group_by(layer,type) |> summarise(total_area_millha = sum(total_area_millha)) |> ungroup() |> 
  group_by(layer) |> 
  mutate( all_frac = total_area_millha / sum(total_area_millha)) |> 
  # Compute the cumulative percentages (top of each rectangle)
  mutate(ymax = cumsum(all_frac),
         # Compute the bottom of each rectangle
         ymin = c(0, head(ymax, n=-1)) ) 

# Compute label position
ex2$labelPosition <- (ex2$ymax + ex2$ymin) / 2
# Compute a good label
# ex2$label <- paste0(str_remove(ex2$type," forest"), "\n", round(ex2$all_frac*100), "%")
ex2$label <- paste0(round(ex2$all_frac*100), "%")
#ex2 |> group_by(layer,type) |> summarise(prop = sum(all_frac))

# Make the plot
ga0 <- ggplot(ex2, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill = type)) +
  theme_void(base_size = 20) +
  geom_rect() +
  coord_polar(theta="y") + # Try to remove that to understand how the chart is built initially
  xlim(c(2, 4)) + # Try to remove that to see how to make a pie chart
  geom_text( x=3.5, aes(y=labelPosition, label=label),colour = "white",size = 7) +
  # geom_label( x=3.5, aes(y=labelPosition, label=label), size=5, colour = "white") +
  scale_fill_manual(values = cols) +
  facet_wrap(~layer,nrow = 2, switch = "x") + 
  labs(title = "") +
  theme(legend.position = "none",title = element_text(hjust = 1))
#ga0
ggsave(plot = ga0,filename = "figures/Figure_pledgedonut.png",width = 8,height = 8,dpi = 400)

# ---- #
# Load, format and plot naturemap coverage
df <- read.csv("resSaves/OverlaySummary_naturemap.csv") |> 
  # mutate(prop30_natural = nm30_natural_millha / total_natural_millha, 
  #        prop30_planted = nm30_planted_millha / total_planted_millha )
  tidyr::pivot_longer(cols = total_natural_millha:nm30_planted_millha) |> 
  rename(variable = name) |> 
  rowwise() |> mutate(prop = value/total_millha) |> 
  tidyr::separate(variable, sep = "_", into = c("what", "type", "unit"))
  
df$layer <- factor(df$layer, levels = c("modis_forestgainsum", "gain_esacci", "Hansen_forestgain"),
                   labels = c("MODIS", "ESA CCI", "Hansen"))
df$type <- factor(df$type, levels = c("natural", "planted"), labels = names(cols))

# Fill missing data with grey
df2 <- df |> filter(what=="nm30") 
df2 <- bind_rows(df2,
                 df2 |> group_by(layer, what) |> summarise(prop = 1 - sum(prop)) |> mutate(type = "Outside")
  )
df2$type <- factor(df2$type, levels = c("Outside", names(cols)))

ga1 <- ggplot(df2, 
       aes(x = layer, y = prop, fill = type)) +
  theme_light(base_size = 20) +
  coord_flip() +
  geom_bar(stat = "identity", show.legend = TRUE, colour = "black") + 
    scale_y_continuous(breaks = pretty_breaks(7)) +
  scale_fill_manual(values = c("Outside" = "grey90", cols)) +
  guides(fill = guide_legend(title = "") ) +
  theme(axis.text.x.bottom = element_text(angle = 0, hjust = 1)) +
  labs(tag = "d", x = "", y = "Proportion", title = "Tree cover gain in\nareas of high conservation value")
ga1
ggsave(plot = ga0,filename = "figures/Figure_pledgedonut.png",width = 8,height = 8,dpi = 400)

# ----- #
# Load, format and plot foodproduction coverage
df <- read.csv("resSaves/OverlaySummary_foodintensity.csv") |> 
  mutate(zone = fct_collapse(zone,
    Low = c("Scattered cropland and grazing"),
    Mixed = c("Mixed and diverse food cultivation"),
    Intense = c("Irrigated and/or intensive food production"),
    Other = c("Areas with little or only subsistence food production", "Urbanized land", "Inland water")
  ) ) |> ungroup() |> 
  group_by(zone,layer,type) |> summarise(area = sum(total_millha)) |> ungroup() |> 
  group_by(layer) |> mutate(prop = area / sum(area))

df$layer <- factor(df$layer, levels = c("modis_forestgainsum", "gain_esacci", "Hansen_forestgain"),
                   labels = c("MODIS", "ESA CCI", "Hansen"))
df$type <- factor(df$type, levels = c("natural", "planted"), labels = names(cols))
df$zone <- factor(df$zone, levels = c("Low", "Mixed","Intense", "Other"))

ga2 <- ggplot(df, 
              aes(x = layer, y = prop, fill = type)) +
  theme_few(base_size = 20) +
  coord_flip() +
  geom_bar(stat = "identity", show.legend = FALSE, colour = "black") + 
  scale_y_continuous(breaks = pretty_breaks(5)) +
  scale_fill_manual(values = c("Outside" = "grey90", cols)) +
  facet_grid(type~zone) +
  guides(fill = guide_legend(title = "") ) +
  theme(axis.text.x.bottom = element_text(angle = 0, hjust = 1)) +
  labs(tag = "d", x = "", y = "Proportion", title = "Tree cover gain in food production areas")
ga2

# Stacked Waterfall plot! 
# https://stackoverflow.com/questions/48259930/how-to-create-a-stacked-waterfall-chart-in-r
# https://www.r-bloggers.com/2019/06/stacked-waterfall-graphs-in-r/
# Going from total overall through each of the food production zones, filled by each

#### SI Figure country analyses ####
# Idea:
# Extract of each country the proportion restored/planted/plantation
# Plot via tenary diagram
# Then focus link on certain outliers and show them in insets
# library(exactextractr)
# data("World",package = 'tmap')
# 
# if(!file.exists('resSaves/country_modalgain.rds')){
#   # Instead of consensus, extract per type and zone a new layer from GEE
#   # Thus averaging the individual area estimates
#   # fml_consensus <- raster('extracts/consensus_forestgain.tif')
#   fml_modis <- raster('extracts/modisgain_zones.tif')
#   fml_area <- raster::area(fml_modis) # km2
#   fml_area <- fml_area * 100 # Convert to ha
#   fml1 <- fml_modis == 1
#   ex1 <- exactextractr::exact_extract(fml1*fml_area, World, fun = 'sum')
#   fml2 <- fml_modis == 2
#   ex2 <- exactextractr::exact_extract(fml2*fml_area, World, fun = 'sum')
#   fml3 <- fml_modis == 3
#   ex3 <- exactextractr::exact_extract(fml3*fml_area, World, fun = 'sum')
#   ex_modis <- bind_rows(
#     data.frame(name = World$name,continent = World$continent, economy = World$economy,
#                income_grp = World$income_grp,
#                value = (ex1), prop = ex1 / (ex1+ex2+ex3), type = 'natural'),
#     data.frame(name = World$name,continent = World$continent, economy = World$economy,
#                income_grp = World$income_grp,
#                value = (ex2), prop = ex2 / (ex1+ex2+ex3), type = 'aided'),
#     data.frame(name = World$name,continent = World$continent, economy = World$economy,
#                income_grp = World$income_grp,
#                value = (ex3), prop = ex3 / (ex1+ex2+ex3), type = 'planted')
#   ) %>% dplyr::mutate(dataset = 'modis')
#   
#   # ESA CCI
#   fml_esacci <- raster('extracts/esaccigain_zones.tif')
#   fml_area <- raster::area(fml_esacci) # km2
#   fml_area <- fml_area * 100 # Convert to ha
#   fml1 <- fml_esacci == 1
#   ex1 <- exactextractr::exact_extract(fml1*fml_area, World, fun = 'sum')
#   fml2 <- fml_esacci == 2
#   ex2 <- exactextractr::exact_extract(fml2*fml_area, World, fun = 'sum')
#   fml3 <- fml_esacci == 3
#   ex3 <- exactextractr::exact_extract(fml3*fml_area, World, fun = 'sum')
#   ex_esacci <- bind_rows(
#     data.frame(name = World$name,continent = World$continent, economy = World$economy,
#                income_grp = World$income_grp,
#                value = (ex1), prop = ex1 / (ex1+ex2+ex3), type = 'natural'),
#     data.frame(name = World$name,continent = World$continent, economy = World$economy,
#                income_grp = World$income_grp,
#                value = (ex2), prop = ex2 / (ex1+ex2+ex3), type = 'aided'),
#     data.frame(name = World$name,continent = World$continent, economy = World$economy,
#                income_grp = World$income_grp,
#                value = (ex3), prop = ex3 / (ex1+ex2+ex3), type = 'planted')
#   ) %>% dplyr::mutate(dataset = 'esacci')
#   
#   # Hansen
#   fml_hansen <- raster('extracts/hansengain_zones.tif')
#   fml_area <- raster::area(fml_hansen) # km2
#   fml_area <- fml_area * 100 # Convert to ha
#   fml1 <- fml_hansen == 1
#   ex1 <- exactextractr::exact_extract(fml1*fml_area, World, fun = 'sum')
#   fml2 <- fml_hansen == 2
#   ex2 <- exactextractr::exact_extract(fml2*fml_area, World, fun = 'sum')
#   fml3 <- fml_hansen == 3
#   ex3 <- exactextractr::exact_extract(fml3*fml_area, World, fun = 'sum')
#   ex_hansen <- bind_rows(
#     data.frame(name = World$name,continent = World$continent, economy = World$economy,
#                income_grp = World$income_grp,
#                value = (ex1), prop = ex1 / (ex1+ex2+ex3), type = 'natural'),
#     data.frame(name = World$name,continent = World$continent, economy = World$economy,
#                income_grp = World$income_grp,
#                value = (ex2), prop = ex2 / (ex1+ex2+ex3), type = 'aided'),
#     data.frame(name = World$name,continent = World$continent, economy = World$economy,
#                income_grp = World$income_grp,
#                value = (ex3), prop = ex3 / (ex1+ex2+ex3), type = 'planted')
#   ) %>% dplyr::mutate(dataset = 'hansen')
#   
#   # Combine all
#   ex <- bind_rows(ex_modis, ex_esacci, ex_hansen)
#   
#   gc();raster::removeTmpFiles(.1)
#   dir.create('resSaves',showWarnings = FALSE)
#   saveRDS(ex, 'resSaves/country_modalgain.rds')
# }

# --- #
data("World")
# ex <- readRDS('resSaves/country_modalgain.rds')
ex <- read.csv('resSaves/OverlaySummary_countries.csv')

# Join in Bonn pledges
pledge <- readxl::read_xlsx('pledges.xlsx')
# Manual reformating
pledge$country[pledge$country=='Scotland'] <- 'United Kingdom'
# pledge$country[pledge$country=='United States of America'] <- 'United States'
pledge$country[pledge$country=="C\u00f4te d'Ivoire"] <- "Ivory Coast"
pledge$country[pledge$country=="Republic of Sudan"] <- "Sudan"
pledge$country[pledge$country=="Tanzania"] <- "United Republic of Tanzania"
# pledge$country[pledge$country=='Central African Republic'] <- 'Central African Rep.'
# pledge$country[pledge$country=="Republic of the Congo"] <- 'Dem. Rep. Congo'
# pledge$country[pledge$country=="Democratic Republic of the Congo"] <- 'Congo'
# pledge$country[pledge$country=="Dominican Republic"] <- 'Dominican Rep.'
pledge$country[pledge$country=="Eswatini"] <- "eSwatini"

assertthat::assert_that(
  length(pledge$country[which(!(pledge$country %in% ex$country_sov))]) == 0
)
#sort(ex$name)

# Join in 
ex <- dplyr::left_join(ex |> dplyr::select(-country), pledge, by = c('country_sov' = 'country'))
# Add income groups
# ex <- dplyr::left_join(ex, World |> dplyr::select(income_grp,sovereignt) |> sf::st_drop_geometry(),
#                        by = c('country_sov' = 'sovereignt'))

# --- #
# Summary stats
# Overall:
ex %>% dplyr::group_by(layer) %>%
  dplyr::summarise(tot = sum(total_area_millha)) %>% 
  dplyr::summarise(m = mean(tot * 1e6),
                   sd = sd( tot * 1e6))
  
# Pledges fullfilled per dataset

pf <- ex %>% 
  # Get only pledging countries
  tidyr::drop_na(pledge_ha) %>% 
  dplyr::mutate(pledge_fulfilled = (total_area_millha*1e6) / pledge_ha ) %>% 
  dplyr::group_by(country_sov, layer, type) %>% 
  dplyr::summarise(pfprop = mean(min(1, pledge_fulfilled,na.rm = TRUE)))

# Spread and check which ones are fullfilled
pf_check <- pf %>% tidyr::spread(key = type,value = pfprop) %>% 
  dplyr::mutate(check = sum(natural,planted) >= 1)

ggplot(pf_check, aes(x = income_grp, y = check) ) +
  geom_bar(stat = 'identity')


df %>% arrange(desc(natural)) %>% head(20)
ggplot(ex, aes(x = type, y = value)) +
  theme_classic() +
  geom_bar(stat = 'identity') +
  facet_wrap(~continent)

# ---------- #
# Figure Idea:
# For each country in bonn pledge
#   Bar plot with average gain across datasets
#   Split by colours and type
#   Names using AT

# Average value per type across datasets
avg_area <- ex %>% dplyr::mutate(prop = if_else(is.nan(prop), 0, prop),
                     value = if_else(is.na(value), 0 , value)) %>%
  # Get only countries that pledged
  dplyr::filter(pledge_ha > 0, dataset %in% c('modis','esacci')) %>% 
  group_by(name, type) %>% 
  dplyr::summarise(
    mean = mean(value),
    sd = sd(value)
  ) %>% ungroup() %>% 
  # Join in pledges again
  dplyr::left_join(., pledge, by = c('name' = 'country') )

# Check for each countries and summarize as follows
# If the combined natural and aided vegetation has reached bonn pledge, set to new status 'reached'
# otherwise show as
# avg_area <-
#   dplyr::left_join(
#     avg_area, avg_area %>% 
#     dplyr::group_by(name) %>% 
#     dplyr::summarise(
#       reached = sum(mean[type=='natural'], mean[type=='aided']) >= pledge_ha,
#       reached_artif = sum(mean) >= pledge_ha
#     ) %>% distinct() %>% ungroup()
# )

# Split up for formatting
avg_area <- avg_area %>% dplyr::select(-sd) %>% 
  tidyr::pivot_wider(names_from = type, values_from = mean)

# Now build new relative estimate
for(r in 1:nrow(avg_area)){
  if(sum(avg_area[r, c('natural','aided')] ) >= avg_area$pledge_ha[r] ){
    # Goal reached
    avg_area$natural[r] <- min(1, avg_area$natural[r] / avg_area$pledge_ha[r])
    avg_area$aided[r] <- (1 - avg_area$natural[r])
    avg_area$planted[r] <- 0
  } else if(sum(avg_area[r, c('natural','aided','planted')]) >= avg_area$pledge_ha[r] ){
    avg_area[r, c('natural','aided')] <- (avg_area[r, c('natural','aided')] / avg_area$pledge_ha[r])
    avg_area$planted[r] <- (1 - sum(avg_area[r, c('natural','aided')]) )
  } else {
    avg_area[r, c('natural','aided','planted')] <- (avg_area[r, c('natural','aided','planted')] / avg_area$pledge_ha[r])
  }
}
assertthat::assert_that(all(avg_area$natural<=1),all(avg_area$planted<=1))
# To longer
avg_area <- tidyr::pivot_longer(avg_area, cols = aided:planted, names_to = 'type',values_to = 'rel_area')

# Relabel
avg_area$type <- factor(avg_area$type,
                        levels = c('natural','aided','planted'),
                        labels = names(cols)
)
# Join in codes
avg_area <- dplyr::left_join(avg_area, ex %>% dplyr::select(name, iso_a3) %>% distinct())

# Rank order
ord <- avg_area %>% group_by(iso_a3) %>% dplyr::summarise(ord = sum(rel_area)) %>% 
  arrange(desc(ord)) %>% 
  dplyr::filter(ord > 0)
# Move those with only natural regeneration to top
ord$iso_a3 <- factor(ord$iso_a3,
                     levels = unique(
                       c(as.character(avg_area$iso_a3[which(avg_area$rel_area==1)]),
                         as.character(ord$iso_a3))
                        )
                      )

# Make plot
g3 <- ggplot(avg_area, 
            aes(x = factor(iso_a3, levels = rev(levels(ord$iso_a3))), y = rel_area, fill = type)) +
  theme_classic(base_size = 18) +
  geom_bar(stat = 'identity', position = 'stack') +
  coord_flip() +
  # Pledge line
  geom_hline(yintercept = 1,linetype = 'dotted', size = 1.25) +
  scale_fill_manual(values = cols) + 
  scale_y_continuous(breaks = pretty_breaks(5),expand = c(0,0) ) + 
  # Switch axes labels
  scale_x_discrete(position = 'top') +
  # Remove axis labels
  #theme(axis.text.x.bottom = element_blank(), axis.ticks.x.bottom = element_blank()) +
  guides(fill = 'none', alpha = 'none') +
  labs(x = '', y = 'Bonn restoration target reached') + theme(axis.title = element_text(size = 24))
g3

ggsave(plot = g3,filename = 'Figure_ranks.png',width = 6,height = 15,dpi = 400)

# -------------------- #
#### SI Figure 1 ####
# Aggregate and visualize the different forest gain estimates to 
# highlight agreement and disagreement among datasets
# Aggregation was done in Google Earth Engine
library(terra)
library(RStoolbox)

ras1 <- rast("extracts/ESACCI_forestsum.tif")
ras2 <- rast("extracts/Hansen_forestgain.tif")
ras3 <- rast("extracts/MODIS_forestsum.tif")
ras1[ras1>0] <- 1
ras2[ras2>0] <- 1
ras3[ras3>0] <- 1
ras2 <- terra::resample(ras2, ras1, method = "near")
ras3 <- terra::resample(ras3, ras1, method = "near")

# Now sum them all
o <- ras1+ras2+ras3
o[o==0] <- NA
o2 <- terra::aggregate(o,fact=10,fun = "modal")
terra::writeRaster(o2,"resSaves/SIFigure1_ModeAgg.tif")

# Load and plot as SI Figure
ras_dis <- raster("resSaves/SIFigure1_ModeAgg.tif")

ras_dis <- ratify(ras_dis)
rat <- levels(ras_dis)[[1]]
rat$cat <- c("1","2","3")
rat$ID <- c(1,2,3)
levels(ras_dis) <- rat

tm <- tm_shape(World,is.master = FALSE, projection = moll) +
  tm_style(style = 'natural') +
  tm_borders(col = "grey20",lwd = .1) + tm_fill(col = 'white',zindex = 1) +
  tm_shape(ras_dis, projection = moll) +
  tm_raster("ESACCI_forestgainsum",  style = 'cat', labels = c("1","2","3"),
            palette = c("lightblue","red","black"), title = "Agreement\n(nr datasets)",legend.hist = F) +
  tm_layout(scale = .65,
            # main.title = "Tree cover gain until 2015",main.title.position = 'center',main.title.size = .75,
            earth.boundary = TRUE, earth.boundary.lwd = 1,
            legend.position = c(.075,.25),#c("center",'bottom'),
            legend.bg.color = "white", legend.bg.alpha = 0, legend.text.size = .6,
            legend.frame = FALSE, #legend.frame = "gray50"
            frame = FALSE, inner.margins = 0, outer.margins = 0, asp = 0, legend.width = 400
  )# + tm_legend(legend.outside=T, legend.outside.position="right")
tmap_save(tm,filename = 'figures/SIFigure_dissimilaritymap.png',width = 1400,height = 900,dpi = 400)
