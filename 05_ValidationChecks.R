library(terra)
library(tidyverse)
library(tidyr)
library(ggplot2)
library(sf)
library(assertthat)

# --- #
p_ll <- '+proj=longlat +datum=WGS84 +no_defs'
e <- c(-180,180,-90,90)

cols <- c("Natural forest" = "#2A9E00",
          "Managed forest" = '#943B60')


# -------------------------------- #
#### 0 - Check Validation set of points ####
# Get original set
test_df <- read.csv("resSaves/validation_sample_random.csv")

# Checked points
check_df <- read.csv("Validation/20230327_validation_forest_restoration_56.csv", sep = ";") |> 
  tidyr::drop_na(sampleid) |> 
  # Reformat the names 
  dplyr::mutate(name = case_when(
    name %in% c("Forest restoration - Is there tree growth over time") ~ "TreeGain",
    name %in% c("Forest restoration - What is the predominant forest management class") ~ "TreeType",
    name %in% c("Forest restoration - What is the forest management of the tree growth") ~ "TreeTypeGain"
  )) |> 
  spread(key = name, value = name.1)

# Now select the relevant columns and remove
check_df <- check_df |> 
  dplyr::arrange(timestamp) |> 
  dplyr::select(sampleid, # sampleid – unique sample site (pixel) identifier
                sample_x, sample_y, # sample_x, sample_y – centroid coordinates for a certain sampleid
                # submissionid – unique submission identifier. 
                # You may have a few submissions per one location (sampleid). Please take the maximum, 
                # which would be the most recent submissionid
                submissionid, 
                TreeGain, TreeType, TreeTypeGain
                ) |> 
  dplyr::group_by(sampleid, sample_x, sample_y, submissionid) |>
  tidyr::fill(TreeGain, TreeType, TreeTypeGain, .direction = "downup") |> 
  distinct() |> ungroup() |> 
  # Group and take the last submitted one always
  dplyr::slice_tail(n = 1, by = sampleid) |>
  distinct() |> 
  # Convert to sf
  sf::st_as_sf(coords = c("sample_x", "sample_y"), crs = sf::st_crs(4326))

# Correct tree gain respectively
check_df$TreeTypeGain[check_df$TreeGain=="Yes" & is.na(check_df$TreeTypeGain)] <- "Not sure"
check_df$TreeGain[check_df$TreeGain == "No" & !is.na(check_df$TreeTypeGain)] <- "Yes" # These should be yes given the type
check_df$TreeTypeGain[is.na(check_df$TreeTypeGain) & check_df$TreeGain == "Not sure"] <- "Not sure"

# Ensure that no duplicates exist
assert_that(anyDuplicated(check_df$submissionid)==0,
            anyNA(check_df$TreeGain) == FALSE,
            anyDuplicated(check_df$sampleid)==0)

# Check
table(check_df$TreeGain)
table(check_df$TreeType)
table(check_df$TreeGain, check_df$TreeTypeGain, useNA = "ifany")

# --------- #
#### 1 - Re-extract the original layers again for doing the validation ####
# Load all the layers

fmlgain <- rast("extracts/combined_forestgain_2000onwards.tif")
names(fmlgain) <- "fml_gain"
fmlgain <- terra::as.factor(fmlgain)

# Get also the Agreement layer. This layers indicates for each grid cell
# how many of the three datasets agree on whether there is forest gain
# Values rank from 1 to 3
dissim <- rast("resSaves/ForestGainAgreement_2000onwards.tif")
names(dissim) <- "Agreement"

# And the individual raster layers of total forest gain per grid cell
# Convert them to binary
ras1 <- rast("extracts/ESACCI_forestgainsum_2000onwards.tif")
ras2 <- rast("extracts/Hansen_forestgain.tif")
ras3 <- rast("extracts/modis_forestgainsum.tif")

# --- #
# Now extract for each
ex <- data.frame(sampleid = check_df$sampleid)

# Extract the values
ex$fml <- terra::extract(fmlgain, check_df, method = "simple", fun = max)[,2]

ex$val_esacci <- terra::extract(ras1, check_df)[,2]
ex$val_hansen <- terra::extract(ras2, check_df)[,2]
ex$val_modis <- terra::extract(ras3, check_df)[,2]


# ---- #
#### 2 - Calculate validation statistics ####
require(caret)

# Join
o <- check_df |> sf::st_drop_geometry() |> left_join(ex) |> 
  dplyr::filter(TreeGain != "Not sure") |> 
  dplyr::mutate(TreeGain = if_else(TreeGain == "Yes", 1, 0) ) |> 
  dplyr::mutate(fml = case_when(fml == 0 ~ "Not sure",
                                fml == 1 ~ "natural",
                                fml == 3 ~ "plantation"))

# -- #
# Validate Esacci
tt1 <- table(o$TreeGain, ifelse(o$val_esacci>0, 1, 0)) |> caret::confusionMatrix(positive = "1",mode = "everything")

# Validate Modis
tt2 <- table(o$TreeGain, ifelse(o$val_modis>0, 1,0)) |> caret::confusionMatrix(positive = "1",mode = "everything")

# Validate Hansen
tt3 <- table(o$TreeGain, ifelse(o$val_hansen>0, 1,0)) |> caret::confusionMatrix(positive = "1",mode = "everything")

oo <- o |> dplyr::filter(fml != "Not sure") 
table(o$TreeType, o$fml) |> caret::confusionMatrix()

