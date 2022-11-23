library(gdalUtils)
library(terra)
library(assertthat)
makeFiles <- function(input, ofname, r = "average", ot = "Int32"){
  assertthat::assert_that(
    all(file.exists(input)),
    is.character(ofname)
  )
  ot <- match.arg(ot, c("Byte","Int16","UInt16","UInt32","Int32","Float32",
                        "Float64","CInt16","CInt32","CFloat32","CFloat64"),several.ok = FALSE)
  
  gdalbuildvrt(gdalfile = input,
               output.vrt = "out.vrt",
               resolution = "highest",
               separate = FALSE,
               r = r)
  
  # Now combine save
  gdal_translate(src_dataset = "out.vrt",
                 dst_dataset = ofname,
                 ot = ot,
                 r = r,
                 co = c("COMPRESS=DEFLATE","PREDICTOR=2","ZLEVEL=9"),
                 verbose = TRUE
  )
  file.remove("out.vrt")
  invisible()
}

# --- #
# Process each extracted file, merging them and then delete it

# Load files from extract folders and project
ll <- list.files("extracts/",full.names = TRUE)
ll <- ll[has_extension(ll, "tif")]

# First land area in m2
ll_land <- ll[grep("landarea",ll)]
makeFiles(ll_land, "extracts/landarea.tif")
sapply(ll_land, function(z) file.remove(z))

# Hansen Forest gain
ll_hansen <- ll[grep("Hansen_forestgain",ll)]
makeFiles(ll_hansen, "extracts/Hansen_forestgain.tif")
sapply(ll_hansen, function(z) file.remove(z))

# Hansen Forest gain
ll_esacci <- ll[grep("ESACCI_forestgain-", ll)]
makeFiles(ll_esacci, "extracts/ESACCI_forestgain.tif")
sapply(ll_esacci, function(z) file.remove(z))

# Hansen forest gain sum
ll_esacci <- ll[grep("ESACCI_forestgainsum", ll)]
makeFiles(ll_esacci, "extracts/ESACCI_forestgainsum.tif")
sapply(ll_esacci, function(z) file.remove(z))
