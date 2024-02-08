#=================================================
# Create new rasters
#=================================================

# 2024-01-12
# Niamh Wall & Peter R.
# P. James and MJ Fortin Labs


# Notes:
# - Aim: Here we use water rasters to augment wetland. However, we augment it by adding 0's throughout


 #===============================
# Libraries
#===============================

library(sf) # sf_1.0-9
library(terra) # Install package terra before "raster" to avoid conflicts. # terra_1.6-17
library(foreach)  #foreach_1.5.2
library(landscapemetrics) # landscapemetrics_1.5.4
library(plyr)  # plyr_1.8.8
library(reshape2) # reshape2_1.4.4
library(raster) # raster_3.6-3


#=================================
# Set folder & files paths
#=================================

#setwd("~/PhD/niamh/")
#setwd("~/niamh_directory/")

#outf11 <- "./output/version2/gis/"

# I created this shp in QGIS with some manual manipulations due to slivers produced when instersecting 
# with UTM 10 km subset
# 1km grid
#shpf1 <- "C:/Users/Peter R/Documents/PhD/niamh/output/version2/gis/utm_1_km_grid_southern_subset1.shp"
#shpf1 <- "~/PhD/niamh/output/version2/gis/utm_1_km_grid_southern_subset3.shp" # no slivers
shpf1 <- "~/PhD/niamh/output/version3/gis/utm_grid_1km_wetland_1800_100x100m_per_lcover_v1.shp"

#shpf2 <- "~/PhD/niamh/data/gis/wetland_2002/_2002WtldExtent.shp"

# This is the OLCC raster which is used a template raster to derive landscape metrics of historic wetlands
#raster1 <- "~/PhD/niamh/data/gis/crop2_OLCC_Vclip_epsg3161.tif"


# This is used as a template to rasterize vectors
#raster3 <- "~/PhD/niamh/output/version2/gis/wetland_1800_per_lc_10km.tif"


# Output folder
#outf1 <- "~/PhD/niamh/output/version3/gis/"
#outf3 <- "~/PhD/niamh/output/version3/data/"
#outf1 <- "~/PhD/niamh/output/version5/gis/"
#outf2 <- "~/PhD/niamh/output/version5/data/"


# Historic & water lanscape metrics (percent land cover)
raster_files1 <- c(
paste0(outf1,"wetland_1800_per_lc_1km.tif"),
paste0(outf1,"wetland_1967_per_lc_1km.tif"),
paste0(outf1,"wetland_1982_per_lc_1km.tif"),
paste0(outf1,"wetland_2002_per_lc_1km.tif") )

raster_files2<- c(
paste0(outf1,"wetland2_1800_per_lc_1km.tif"),
paste0(outf1,"wetland2_1967_per_lc_1km.tif"),
paste0(outf1,"wetland2_1982_per_lc_1km.tif"),
paste0(outf1,"wetland2_2002_per_lc_1km.tif"))


# These prefix and suffixes are need to create files with the correct labels
#prefix1 <- "utm_grid_10km_"
#prefix1 <- "utm_grid_5km_"
prefix1 <- "utm_grid_1km_"

#suffix1 <- "_10km"
#suffix1 <- "_5km"
suffix1 <- "_1km"



wetland1 <- terra::rast(raster_files1)
wetwaterland1 <- terra::rast(raster_files2)

background1 <- vect(shpf1)

r1Template <- wetland1[[1]]

# rasterize(background1, r1Template, field=0, background=NA, touches=FALSE, update=FALSE,  cover=FALSE, filename=paste0(outf11, "background", "_per_lc", suffix1,".tif"), overwrite=TRUE) # fun= when pts

background1r <- rast("~/PhD/niamh/output/version5/gis/background_per_lc_1km.tif")

background1r <- background1r+0.01


plot(wetland1[[1]], main="Wetland 1800")
dev.new(); plot(wetwaterland1[[1]], main="Wetland 1800 + water")

wetValue <- c(500, 600, 700, 800)

# Only historic wetlands
rwetL10 <- foreach(i=1:nlyr(wetland1)) %do%  {

        wetwaterland2 <- ifel(wetwaterland1[[i]]>=0, 0, wetwaterland1[[i]])
                  
         temp10 <-  app(c(wetland1[[i]], wetwaterland2), fun="first", na.rm=TRUE) 
         
         
                          
                  }

                  
rwetL11 <- foreach(i=1:nlyr(wetland1)) %do%  {

                       
         temp10 <-  app(c(wetland1[[i]], background1r), fun="first", na.rm=TRUE) 
         
         
                          
                  }



dev.new()
plot(rwetL10[[i]], main="Wetland 1800 + 0s")
hist(wetland1[[1]])
hist(rwetL10[[1]])

dev.new(); plot(rwetL11[[1]], main="Wetland 1800 + background of 0s")

#=============================================
# Create CSV and raster from landscape metrics
#=============================================

# names for the rasters to be created below
# wetland2 are water augmented rasters
#outname1 <- c("wetland3_1800", 
#"wetland3_1967",  
#"wetland3_1982",
#"wetland3_2002" 
# )
# 
#outname1 <- c("wetland4_1800", 
#"wetland4_1967",  
#"wetland4_1982",
#"wetland4_2002" 
# )
# 
# outname1 <- c("wetland5_1800", 
#"wetland5_1967",  
#"wetland5_1982",
#"wetland5_2002" 
# )
 
  outname1 <- c("wetland6_1800", 
"wetland6_1967",  
"wetland6_1982",
"wetland6_2002" 
 )
 
 
foreach (i=1:length(rwetL11))  %do%  {
terra::writeRaster(rwetL11[[i]], filename=paste0(outf1, outname1[i], "_per_lc", suffix1,".tif"), overwrite=TRUE) }

#--------------------------------
# Augmented wetland with OLCC, wetland6 series
foreach (i=1:length(rwetL10))  %do%  {
terra::writeRaster(rwetL10[[i]], filename=paste0(outf1, outname1[i],".tif"), overwrite=TRUE) }


