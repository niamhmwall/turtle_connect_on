#=================================================
# Add Elevation metrics to Shortest Paths 
#=================================================

# Turtle Landscape Genetics Project  (Naimh's)
# 2024-02-10
# Code authors: Niamh W. & Peter R.
# P. James and MJ Fortin Labs

# Aim: Produce elevation/slope stats using shortest path derived from historic wetland data.


#===============================
# Libraries
#===============================

library(sf)
library(terra) # Install package terra before "raster" to avoid conflicts
library(foreach)
library(sqldf)
#library(landscapemetrics)
#library(plyr)
#library(reshape2)
#library(raster)
#library(gdistance)

#library(plyr)
#library(reshape2)

#sessionInfo()
# R version 4.1.1 (2021-08-10)

#other attached packages:
# [1] sqldf_0.4-11    RSQLite_2.2.19  gsubfn_0.7      proto_1.0.0     reshape2_1.4.4 
# [6] plyr_1.8.8      gdistance_1.6   Matrix_1.5-4    igraph_1.3.5    raster_3.6-3   
#[11] sp_1.4-6        foreach_1.5.2   terra_1.6-17    sf_1.0-9       
           
#=================================
# Set folder & files paths
#=================================

# Set wording directory
#setwd("~/PhD/niamh/output/version10/")
# Niamh's
#setwd()

# Peter's data path
fpath <- "~/PhD/niamh/output/version10/"
# Niamh's'
#fpath <- "C:/Users/Niamh/Documents/PhD/niamh/output/version2/gis"

fpath2 <- "~/PhD/niamh/misc/"
 

# Output folder for rasters

outf1 <- "~/PhD/niamh/output/version10/gis/"  # 
outf2 <- "~/PhD/niamh/output/version10/data/"  # 

# These prefix and suffixes are need to create files with the correct labels
#prefix1 <- "utm_grid_10km_"
#prefix1 <- "utm_grid_5km_"
#prefix1 <- "utm_grid_1km_"

#suffix2 <- "_10km_jenks1"
#suffix2 <- "_5km_jenks1"
#suffix2 <- "_1km_jenks1"
suffix1 <- "_1km"
#suffix1 <- "_1km_jenks1"

#shp
shpf1 <- "gis/tur_shpath_1km_buffer_v1.shp"

# rasters
raster1 <- "~/PhD/niamh/output/version10/gis/southern_ont_1km_elev_v1.tif"
raster2 <- "~/PhD/niamh/output/version10/gis/southern_ont_1km_slope_v1.tif"
raster3 <- "~/PhD/niamh/output/version10/gis/southern_ont_1km_roughness_v1.tif"


#===================================
# Local raster creation
#===================================
# Notes:
# - You need to run gdistance_metrics_data_frame_v5.R first so that the object rWet is created
# - You may want to to cummulative sum of rWetRcl instead.

# Load elevation data is 90 x 90 m. OLCC is 15 m but we aggregated to 100m.

#----------------------------------------
# local computation to create rasters
#----------------------------------------
# Note: the following objects need to be created once locally and then used below

#raster1 <- "C:/Users/Peter R/Documents/data/gis/srtm/southern_on/SRTMGL3_NC.003_SRTMGL3_DEM_doy2000042_aid0001.tif"
#
#elevr <- rast(raster1)
#
#elevr <- aggregate(elevr, fact=11)
##names(elevr) <- "elevation"
#
#writeRaster(elevr, paste0(outf1, "southern_ont_1km_elev_v1.tif") , overwrite=TRUE)
#
#sloper <- terrain(elevr, v="slope", neighbors=8, unit="degrees", filename=paste0(outf1, "southern_ont_1km_slope_v1.tif") , overwrite=TRUE)
#
#roughr <- terrain(elevr, v="roughness", neighbors=8, unit="degrees", filename=paste0(outf1, "southern_ont_1km_roughness_v1.tif"), overwrite=TRUE)

#dev.new(); plot(elevr)
#dev.new(); plot(terr)
#dev.new(); plot(roughr)


#==================================
# Load data 
#==================================

elevr <- rast(raster1)
sloper <- rast(raster2)
roughr <- rast(raster3)

elevrStk <- c(elevr, sloper, roughr)

g1 <- terra::vect(shpf1)
grids1 <- sf::st_read(shpf1)

#g1 <- g1[g1$rlyr %in% c(4,8),]
#grids1 <- grids1[grids1$rlyr %in% c(4,8),]

# Reproject sf object
grids1Pj <- st_transform(sf::st_as_sf(g1), crs(elevrStk))


# Notes:
# 1) Given we are dealing with elevation and this does not change over time (or changes little) I could use these elevation-related metric wh al historical wetland Shortest Paths.
# 2) No landscape metrics needed her as we are dealing with single continuos variables.

elevM <- foreach (i= 1:nlyr(elevrStk), .inorder=TRUE, .packages = "foreach")   %do% {
         
        temp1 <- extract(elevrStk[[i]], grids1Pj, fun=mean, bind=TRUE)
        as.data.frame(temp1)
         
         }

#class(elevM)
#class(elevM[[1]])
#length(elevM)
dim(elevM[[3]])
#head(elevM[[3]])

names(elevM) <- c("t1", "t2", "t3")
 
elevDf <- with(elevM, sqldf("SELECT t1.*, t2.slope, t3.roughness FROM t1 JOIN  t2 ON t1.rid=t2.rid JOIN t3 ON t1.rid=t3.rid"))

#dim(elevDf)
#head(elevDf)
#names(elevDf) <- c("rid", "rlyr", "path_id", "elevation", "slope", "roughness")


saveRDS(elevDf, paste0(outf2, "elevDf_", suffix1, ".rds") )



 
