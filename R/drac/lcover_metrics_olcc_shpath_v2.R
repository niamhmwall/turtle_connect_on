#=================================================
# Wetland landscape metrics and zonal stats
#=================================================

# 2024-02-08
# Niamh Wall & Peter R.
# P. James and MJ Fortin Labs

# Aim: Produce landscape stats using several wetland & wetland+water raster layers
# Notes
# - This script uses the grids from the south west section of Ontario UTM Grids 1x1 km data set (n polygons ~ 179K)
# - We combined "Clear Open Water" (code #1) and Turbid Water (code #2)" to produce raster layers merging wetland and water land cover cells
# - The 1km landscape metrics took about 30 hours to complete using Fortin Lab desktop.
# - This code is for running on DRAC (cloud-computing)


#=====================================
# Set up time
#=====================================
# This is just needed for benchmarking
start.time <- Sys.time()
start.time




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
library(doParallel)
library(stlplus)

#sessionInfo()
# R version 4.1.1 (2021-08-10)

#other attached packages:
# [1] landscapemetrics_1.5.4 sqldf_0.4-11           RSQLite_2.2.19         gsubfn_0.7            
# [5] proto_1.0.0            reshape2_1.4.4         plyr_1.8.8             gdistance_1.6         
# [9] Matrix_1.5-4           igraph_1.3.5           raster_3.6-3           sp_1.4-6              
#[13] foreach_1.5.2          terra_1.6-17           sf_1.0-9                              


#=================================
# Set folder & files paths
#=================================

#setwd("~/PhD/niamh/")
#setwd("~/niamh_directory/")
setwd("~/projects/def-mfortin/georod/data/turtle_connect_on/")

#outf11 <- "./output/version2/gis/"
#outf11 <- "C:/Users/Peter R/Documents/PhD/niamh/output/version5"


#dataf <- "~/projects/def-mfortin/georod/data/turtle_connect_on/" # data folder

shpf1 <- "gis/tur_shpath_1km_buffer_v1.shp" # no slivers
#shpf1 <- "C:/Users/Peter R/Documents/PhD/niamh/output/version5/gis/tur_shpath_1km_buffer_v1.shp"



# This is the OLCC raster which is used a template raster to derive landscape metrics of historic wetlands
raster1 <- "gis/olcc_reclass_v1.tif"
#raster1 <- "C:/Users/Peter R/Documents/PhD/niamh/output/version5/gis/olcc_reclass_v1.tif"


# This is used as a template to rasterize vectors
#raster3 <- "gis/wetland_1800_per_lc_10km.tif"

# Output folder
outf1 <- "output/version10/data/"
outf2 <- "output/version10/gis/"


# These prefix and suffixes are need to create files with the correct labels

prefix1 <- "spath_buffer_"

suffix1 <- "1km"



#========================================
# Parallel processing settings
#========================================

# Use the environment variable SLURM_CPUS_PER_TASK to set the number of cores.
# This is for SLURM. Replace SLURM_CPUS_PER_TASK by the proper variable for your system.
# Avoid manually setting a number of cores.
ncores = Sys.getenv("SLURM_CPUS_PER_TASK") 

registerDoParallel(cores=ncores)# Shows the number of Parallel Workers to be used
print(ncores) # this how many cores are available, and how many you have requested.
#getDoParWorkers()# you can compare with the number of actual workers

timeSleep <- 3


#==================================
# Load data 
#==================================

g1 <- terra::vect(shpf1)
grids1 <- sf::st_read(shpf1)

g1 <- g1[g1$rlyr==4,]
grids1 <- grids1[grids1$rlyr==4,]


r3 <- terra::rast(raster1) # This is the fixed OLCC cropped raster. This is needed to derived metrics below. This raster is used as a template.



#==================================
# Prepare Wetland + water raster
#==================================


# Reproject sf object
grids1Pj <- st_transform(sf::st_as_sf(g1), crs(r3))
#grids1Pj <- st_transform(sf::st_as_sf(g1), crs(r3))

rwetL40 <- r3
 

#=========================================
# Calculate landscape metrics 
#=========================================


# Layers 1:4 are only wetland. Layers 5:8 are wetland augmented with OLCC water land cover classes 
#landM <- foreach(i=1:nlyr(rwetL40), .inorder=TRUE, .packages = "foreach") %do%
#                  sample_lsm(raster::raster(rwetL40[[i]]), grids1Pj, plot_id=grids1Pj$"path_id", 
#                      what = c("lsm_c_pland", "lsm_c_te", "lsm_l_np", "lsm_l_pd", "lsm_l_ta"))
                      
landM <- foreach(i=1:nlyr(rwetL40), .inorder=TRUE, .packages = "foreach") %dopar%
                  sample_lsm(raster::raster(rwetL40[[i]]), grids1Pj, plot_id=grids1Pj$"path_id", 
                      what = c("lsm_c_pland"))

#length(landM)


Sys.sleep(timeSleep)

saveRDS(landM, paste0(outf1,prefix1, suffix1,"_landM", ".rds"))


#=============================================
# Create CSV and raster from landscape metrics
#=============================================

# Note: always check that raster have been created correctly. raster 5:8 were given me issues for some reason.
foreach(i=1:length(landM)) %do% {


# Turn list into a data frame
landMdf <- ldply(landM[i], data.frame, .id="raster_name")

      
# subset long df frame
landMdf2 <- landMdf[which(landMdf$metric=='pland'), c(3:8)]
landMdf2$lc_class <- paste0("LC",landMdf2$class)
#head(landMdf2Class)
#summary(landMdf2)

# Restructure the data
landMdf2 <- dcast(landMdf2, plot_id  ~ lc_class, value.var="value")
#head(landMdf2)
#summary(landMdf2)

#landMdf2 <- landMdf2[, c(1:4)] # keep needed columns


# create a new variable with the same name of the shp unique ID
# landMdf2$"OGF_ID" <- landMdf2$plot_id # create new field
landMdf2$"path_id" <- landMdf2$plot_id


#Merge spatial grids with landscape metrics. There should be 1960 rows as our grid shp has 1960 grids.
#landMdf2Sp <- merge(grids1Pj, landMdf2, by = "OGF_ID")

landMdf3 <- merge(st_drop_geometry(grids1Pj[,1:3]), landMdf2, by = "path_id")

saveRDS(landMdf3, paste0(outf1,prefix1, suffix1,"_landMdf3",".rds"))

# create csv
write.csv(landMdf3, paste0(outf1, prefix1, suffix1, "olcc", "_v1.csv"), row.names=FALSE)


        }       


end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken