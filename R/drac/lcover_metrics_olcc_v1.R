#=================================================
# Wetland landscape metrics and zonal stats
#=================================================

# 2024-02-03
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

#setwd("~/PhD/niamh/output/version5/")
#setwd("~/niamh_directory/")
setwd("~/projects/def-mfortin/georod/data/turtle_connect_on/")

#outf11 <- "./output/version2/gis/"


#dataf <- "~/projects/def-mfortin/georod/data/turtle_connect_on/" # data folder

# I created this shp in QGIS with some manual manipulations due to slivers produced when instersecting 
# with UTM 10 km subset
# 1km grid
#shpf1 <- "C:/Users/Peter R/Documents/PhD/niamh/output/version2/gis/utm_1_km_grid_southern_subset1.shp"
#shpf1 <- "gis/utm_1_km_grid_southern_subset3.shp" # no slivers
shpf1 <- "~/PhD/niamh/output/version5/gis/tur_sites_5km_buffer_v2.shp" 

shpf2 <- "gis/wetland_2002/_2002WtldExtent.shp"

# This is the OLCC raster which is used a template raster to derive landscape metrics of historic wetlands
raster1 <- "gis/crop2_OLCC_Vclip_epsg3161.tif"


# This is used as a template to rasterize vectors
raster3 <- "gis/wetland_1800_per_lc_10km.tif"

# Output folder
outf1 <- "output/version10/data/"
#outf3 <- "~/turtle_connect_on/output/version10/data/"


# Historic wetlands
#raster_files1 <- c(
#"gis/historic_w1800_100m_v2.tif",
#"gis/wetland_1967.tif",
#"gis/wetland_1982.tif",
#"gis/wetland_2002.tif")
#
## historic wetalnds augmented with OLCC wetland (has NAs)
#raster_files2<- c(
#"gis/wetland6_1800.tif",
#"gis/wetland6_1967.tif",
#"gis/wetland6_1982.tif",
#"gis/wetland6_2002.tif")


# These prefix and suffixes are need to create files with the correct labels
#prefix1 <- "utm_grid_10km_"
#prefix1 <- "utm_grid_5km_"
prefix1 <- "buffer_10km_"

#suffix1 <- "_10km"
#suffix1 <- "_5km"
suffix1 <- "_olcc"



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
#grids1 <- sf::st_read(shpf1)
#dim(g1)    # 1x1 km grid with no slivers: 178060 ; 1x1 km grid: 179106; 10x10 km: 1960 features

r3 <- terra::rast(raster1) # This is the fixed OLCC cropped raster. This is needed to derived metrics below. This raster is used as a template.

#wetland1 <- terra::rast(raster_files1)
#wetland2 <- terra::rast(raster_files2) # These are the historic augmented with OLCC wetland


#==================================
# Prepare Wetland + water raster
#==================================
 
# Run the following lines before calculating land cover metrics. 
ext1 <- ext(1060020, 1790520, 11600687, 12200990) #Create extent object. Note the order: xmin, xmax, ymin, ymax
r3 <- extend(r3, ext1) # modify extent
#r3 <- subst(r3, 0, NA) # subsititute 0s for NAs

# Reproject sf object
grids1Pj <- st_transform(sf::st_as_sf(g1), crs(r3))

#rm(grids1)


#=========================================
# Reclassify OLCC raster (template raster)
#=========================================

# Reclassify OLCC raster. This is a must to get the right percentages. OLCC raster is used as a template to trick the landscape metrics function (landscapemetrics pkg) to believe the historic raster had values other than wetland. 
# Note: OLCC has values 247 & 157 which may be errors.

#-------------------------------------------------------------------------------
# Workflow to augment Historical wetlands with OLCC 2009 land cover raster
#-------------------------------------------------------------------------------

#-----------------------------------------
# All land cover matrix
#Ok so I think we could just combine "Clear Open Water" (code #1) and Turbid Water (code #2)
#rclFile <- read.csv("C:/Users/Peter R/github/turtle_connect_on/misc/olcc_classes_reclass2024_v2.csv")
rclFile <- read.csv("misc/olcc_classes_reclass2024_v2.csv")
rclM1 <- (as.matrix(rclFile[,2:3]))

# m <- c(0, 2, 10, 2, 247, 0)

# rclM2 <- matrix(m, ncol=3, byrow=TRUE)

 r3Rcl1 <- classify(r3, rclM1, include.lowest=FALSE, right=TRUE)
 #freq( r3Rcl1)

#global(r3Rcl1, fun="isNA")
#global(r3Rcl2, fun="isNA")

 rwetL40 <- r3Rcl1 
 
#write reclassified raster
#writeRaster(r3Rcl1, paste0(outf1, "olcc_reclass_v1.tif"), overwrite=FALSE) 
 
 
#--------------------------------------------------------------------
# Wetland matrix (augmented OLCC 2009)
# m <- c(0, 4, 0, 4, 8, 1, 8, 247, 0 )
# rclM3 <- matrix(m, ncol=3, byrow=TRUE)

# olcc2 <- classify(r3, rclM3, include.lowest=FALSE, right=TRUE)


#wetValue <- c(500, 600, 700, 800)


 
 # Save as raster stack to ease future analysis
 #writeRaster(rast(rwetL40), paste0(outf1, "his_wetland_stack_v6.tif"), overwrite=FALSE)
 #rwetL40 <- rast(paste0("gis/", "his_wetland_stack_v6.tif"))
 
 class(rwetL40)

#global(rwetL40[[3]], fun="notNA")
#global(rwetL40[[7]], fun="notNA")
#freq(rwetL40[[3]])
#freq(rwetL40[[7]])
 

#=========================================
# Calculate landscape metrics 
#=========================================


# Layers 1:4 are only wetland. Layers 5:8 are wetland augmented with OLCC water land cover classes 
#landM <- foreach(i=1:nlyr(rwetL40)) %do%
#                  sample_lsm(raster::raster(rwetL40[[i]]), grids1Pj[1,], plot_id=grids1Pj[1,]$"OGF_ID", 
#                      what = c("lsm_c_pland", "lsm_c_te", "lsm_l_np", "lsm_l_pd", "lsm_l_ta"))
#                      
landM <- foreach(i=1:nlyr(rwetL40)) %do%
                  sample_lsm(raster::raster(rwetL40[[i]]), grids1Pj, plot_id=grids1Pj$"rid", 
                      what = c("lsm_c_pland", "lsm_c_te", "lsm_l_np", "lsm_l_pd", "lsm_l_ta"))

length(landM)
class(landM)
dim(landM[[1]])
head(landM[[1]])
tail(landM[[1]])
unique(landM[[1]]["plot_id"])
summary(as.numeric(landM[[1]]["plot_id"]))

# First 4 rasters are augmented with OLCC wetland and last 4 rasters are augmented with OLCC wetland and water
names(landM) <- c("HISWET_1800","HISWET_1967","HISWET_1982","HISWET_2002","HISWET2_1800","HISWET2_1967","HISWET2_1982", "HISWET2_2002")

Sys.sleep(timeSleep)

saveRDS(landM, paste0(outf1,"landM",suffix1,".rds"))

#landM <- landM_1km

#=============================================
# Create CSV and raster from landscape metrics
#=============================================

lcdf1 <- (landM[[1]])

head(lcdf1[lcdf1$"plot_id"=="1",], 20)

landM[[1]][which(landM[[1]]["plot_id"=="1",]),]


# names for the rasters to be created below
# wetland2 are water augmented rasters
outname1 <- c("wetland_1800", 
"wetland_1967",  
"wetland_1982",
"wetland_2002", 
"wetland2_1800", 
"wetland2_1967" , 
"wetland2_1982" ,  
 "wetland2_2002" )
 
fieldWet <- rep(c("LC500", "LC600", "LC700", "LC800"), 2)


# raster template to rasterize
# This is used a template to rasterize vectors. Make sure you use the right resolution

# 10 km template
r1Template <- rast(raster3)
#r1Template <- rast(raster3)
# 5km template
#r1Template<- disagg(r1Template, 2, method="bilinear") #, filename="", ...)
# 1km template
r1Template <- disagg(r1Template, 10, method="bilinear") #, filename="", ...)

# Read 1km landscape metrics df
#landM <- readRDS(paste0(outf1,"landM_1km.rds"))


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
landMdf2$"rid" <- as.numeric(landMdf2$plot_id) # create new field


#Merge spatial grids with landscape metrics. There should be 1960 rows as our grid shp has 1960 grids.
#landMdf2Sp <- merge(grids1Pj, landMdf2, by = "rid")

landMdf3 <- merge(st_drop_geometry(grids1Pj[,1:3]), landMdf2, by = "rid")

# create csv
write.csv(landMdf3, paste0(outf1, prefix1, "olcc", "_per_lcover_v1.csv"), row.names=FALSE)

# create shp
#st_write(landMdf2Sp, paste0(outf1, prefix1, outname1[i], "_100x100m_per_lcover_v1.shp"), overwrite=TRUE, append=FALSE)  

# rasterize vector

# rasterize UTM grid polygons and save as tif. 
# You need a raster to serve as template.
#rasterize(vect(landMdf2Sp), r1Template, field=fieldWet[i], background=NA, touches=FALSE, update=FALSE,  cover=FALSE, filename=paste0(outf1, outname1[i], "_per_lc", suffix1,".tif"), overwrite=TRUE) # fun= when pts

        }       


head(landMdf3) # same object for all species and layers

ShPathGenDistL50 <- foreach (i=1:3)  %do% {
         foreach (j=1:8) %do% {

 df1 <- ShPathGenDistL20[[i]][[j]]

 sqldf("SELECT t1.*, t2.LC1, t2.LC2, t2.LC3, t2.LC4, t2.LC5, t2.LC6, t2.LC7, t2.LC8 
FROM df1 t1 LEFT JOIN  (SELECT rid, site_pair1 AS path_id, LC1, LC2, LC3, LC4, LC5, LC6, LC7, LC8 FROM landMdf3
UNION ALL SELECT rid, site_pair2 AS path_id, LC1, LC2, LC3, LC4, LC5, LC6, LC7, LC8 FROM landMdf3) t2 ON t1.path_id=t2.path_id"
)
}
}

dim(ShPathGenDistL50[[i]][[j]])
dim(ShPathGenDistL20[[i]][[j]])

head(ShPathGenDistL50[[i]][[j]])
head(ShPathGenDistL20[[i]][[j]])


dim(test1)

#end.time <- Sys.time()
#time.taken <- end.time - start.time
#time.taken