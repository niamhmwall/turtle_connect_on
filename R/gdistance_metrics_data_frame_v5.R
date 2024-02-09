#=====================================================
# Create Gdistance Data Frame - Niamh's Turtle Project
#=====================================================

# 2023-12-14
# Code authors: Niamh W. & Peter R.
# P. James and MJ Fortin Labs

# Aim: Produce gdistance stats using historic wetland data

# Tutorial:
#    https://bookdown.org/hhwagner1/LandGenCourse_book/WE_10.html#WE_10


#===============================
# Libraries
#===============================

library(sf)
library(terra) # Install package terra before "raster" to avoid conflicts
library(foreach)
#library(landscapemetrics)
#library(plyr)
#library(reshape2)
library(raster)
library(gdistance)
#library(rgeos))

library(plyr)
library(reshape2)
library(classInt)

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
#setwd("C:/Users/Peter R/Documents/PhD/niamh/")
# Niamh's
#setwd()

# Peter's data path
#fpath <- "C:/Users/Peter R/Documents/PhD/niamh/output/version3/gis"
# Niamh's'
#fpath <- "C:/Users/Niamh/Documents/PhD/niamh/output/version2/gis"
# Project data path
fpath <- "~/PhD/niamh/output/"
fpath2 <- "~/PhD/niamh/misc"


# Output folder for rasters
#outf0 <- "~/PhD/niamh/output/version2/gis/"  # For spatial data, 10 km
#outf1 <- "~/PhD/niamh/output/version3/gis/"  # For spatial data, 5 km
#outf2 <- "~/PhD/niamh/output/version3/img/"  # For images, figures
#outf3 <- "~/PhD/niamh/output/version3/data/" # For non-spatial data
#outf4 <- "~/PhD/niamh/output/version4/gis/"  # For spatial data, X km, jenks
#outf5 <- "~/PhD/niamh/output/version4/data/"  # For spatial data, X km, jenks
#outf1 <- "~/PhD/niamh/output/version6/gis/"  # 
#outf2 <- "~/PhD/niamh/output/version6/data/"  # 
outf1 <- "~/PhD/niamh/output/version10/gis/"  # 
outf2 <- "~/PhD/niamh/output/version10/data/"  # 


# These prefix and suffixes are need to create files with the correct labels
#prefix1 <- "utm_grid_10km_"
#prefix1 <- "utm_grid_5km_"
#prefix1 <- "utm_grid_1km_"

#suffix1 <- "_10km"
#suffix <- "_5km"
suffix1 <- "_1km"

#suffix2 <- "_10km_jenks1"
#suffix2 <- "_5km_jenks1"
#suffix2 <- "_1km_jenks1"

#===================================
# Read data
#===================================

# Original 10 km 
#rastfiles2 <- list.files(path=outf0, pattern = "wetland_.*per_lc_10km.tif$", full.names = TRUE) #

 
# 5 km 
#rastfiles2 <- list.files(path=outf1, pattern = "wetland_.*per_lc_5km.tif$", full.names = TRUE) #

# 1 km augmented
rastfiles2 <- list.files(path=outf1, pattern = "wetland.*per_lc_1km.tif$", full.names = TRUE) #

#rastfiles2 <- rastfiles2[5:12]

# Create a new raster stack to run gdistance.
rWet <- rast(rastfiles2)
nlyr(rWet)


#=====================================================================
# Reclassify wetland raster using natural breaks (jenks, fisher-jenks)
#=====================================================================

# Skip this part if jenks not need

val1 <- terra::values(rWet[[1:4]])  # We used 1800 to 2002 as the reference
# summary(val1[, 4])
#length(val1)
#hist(val1)
#summary(val1)
#length(as.vector(val1, na.rm=TRUE))
summary(as.vector(val1))
val1 <- as.vector(val1)

# Get class interval using Jenks/Fisher-Jenks (i.e., natural breaks)
# Fisher-Jenks runs faster for n>3000. Note that when n is large a 10% sample is used.
# Hence, the breaks are not equal each time you run classIntervals().
# breaks need to be added manually to object m below

#val1Rcl <- classIntervals(val1, 3, style = "jenks", rtimes = 3, 
#               #intervalClosure = c("left", "right"), 
#               dataPrecision = NULL,
#               warnSmallN = TRUE, warnLargeN = TRUE, largeN = 3000L, samp_prop = 0.1,
#               #gr = c("[", "]")
#               )

val1Rcl <- classInt::classIntervals(val1, 3, style = "fisher", #rtimes = 3, 
                          #intervalClosure = c("left", "right"), 
                          dataPrecision = NULL,
                          warnSmallN = TRUE, warnLargeN = TRUE, largeN = 3000L, samp_prop = 0.1,
                          #gr = c("[", "]")
              )

#class(val1Rcl)
val1Rcl$brks
round(val1Rcl$brks, 1)

# 10 km
#val1Rcl$brks
#[1]  0.01981768 20.92833424 52.62652206 98.97857666
#[1]   0.00000  18.61901  59.29481 100.00000  # only with rWet[[1]]
#[1]   0.00000  15.60910  54.26854 100.00000  # with rWet[[1]]
#m <- c(0, 20.9, 1000, 20.9, 52.6, 100, 52.6, 100, 10)

# 5 km crete breaks manually using  object val1Rcl$brks. 
#m <- c(0, 20.8, 10, 20.8, 54.8, 100, 54.8, 100, 1000)

# 1 km crete breaks manually using  object val1Rcl$brks.
#m <- c(0, 25.6, 10, 25.6, 63.8, 100, 63.8, 100, 1000)
# [1]   0.9803922  21.3884430  57.3714085 100.0000000 # version 8 OLCC augmented  
#m <- c(0, 15.6, 10, 15.6, 54.3, 100, 54.3, 100, 1000) 

wgts <- c(10, 100, 1000)
val1Rcl2 <- c(0, round(val1Rcl$brks, 1)[2], round(val1Rcl$brks, 1)[3], 100)
m <-  c(val1Rcl2[1], val1Rcl2[2], wgts[1], val1Rcl2[2], val1Rcl2[3], wgts[2], val1Rcl2[3], val1Rcl2[4], wgts[3])

#m <- c(0, 21.4, 10, 21.4, 57.4, 100, 57.4, 100, 1000) 

rclM1 <- matrix(m, ncol=3, byrow=TRUE)


# Reclassify raster, all layers at once
rWetRcl <- classify(rWet, rclM1, include.lowest=TRUE, right=TRUE)
#freq(rWetRcl[[1]])
#freq(rWetRcl)


#plot(rWetRcl[[1]])
#dev.new(); plot(rWetRcl[[4]])

rWet <- rWetRcl

# -- END skip 


#===================================
# Load sites (points) 
#===================================

# Note: PR created new sites CSVs. Some of the sites have been moved using 1800 wetland as reference.
# This was done to avoid dropping to many sites.

#pts <- sf::st_read(paste0(fpath2, "/Site_samplingloc_June23.csv"), options=c("X_POSSIBLE_NAMES=Longitude","Y_POSSIBLE_NAMES=Latitude"), crs=4326)

#pts <- sf::st_read(paste0(fpath2, "/Site_samplingloc_Oct20_2023.csv"), options=c("X_POSSIBLE_NAMES=Longitude","Y_POSSIBLE_NAMES=Latitude"), crs=4326)

pts <- sf::st_read(paste0(fpath2, "/sites_1km_v3.csv"), options=c("X_POSSIBLE_NAMES=Longitude","Y_POSSIBLE_NAMES=Latitude"), crs=4326)

#plot(st_geometry(pts))

# create list with points
pts_pjL <- list()

# 1) SPTU
# Leave some sites out. 
pts_pjL[[1]] <- sf::st_transform(pts[!(pts$Site %in% c('EL', 'GB1','GB2','GB3','GB4', 'BC','BC1', 'ALG', 'HU')), ], crs(rWet))

# 2) BLTU
# Sites to keep
pts_pjL[[2]] <- sf::st_transform(pts[(pts$Site %in% c('LH1','HC', 'EO1','EO', 'ALG', 'HU', 'GB4', 'GB3', 'LE3', 'BC1', 'LE1')),], crs(rWet))

# 3) SNTU
# Sites to keep
pts_pjL[[3]] <- sf::st_transform(pts[(pts$Site %in% c('LH1', 'BP', 'LE1', 'HC', 'LH2', 'GB2', 'EO', 'BC', 'GH', 'KAW', 'N', 'ALG', 'HU')), ], crs(rWet))

                                 
# Create species specific sites list  with  SpatialPointsDataFrames
SpSitesL <- foreach (i=1:length(pts_pjL)) %do% {
as(pts_pjL[[i]][, c(1,3:4)], "Spatial") # convert to SpatialPointsDataFrame)
}


#plot(pts2$geometry)
#pts_pj <- sf::st_transform(pts2, crs(rWet))
#class(pts_pj)
#crs(pts_pj, proj=T)

#st_write(pts, paste0(outf1, "sites_v1.shp"), overwrite=T)


#===============================
# Calculating Least-Cost Path
#===============================

#class(rWet[[1]])
#class(rWetRcl)
#global(rWet[[1]], fun="isNA")

#rWet0 <- rWet
# Scale up raster if needed
#rWet <- ifel(rWet > 0.01, rWet^2, rWet)
#rWet2 <- ifel(is.na(rWet), 0.1, rWet)
#rWet2 <- ifel(is.na(rWetRcl), 1, rWetRcl)      # version 9
#rWet2 <- rWet2^2
rWet2 <- rWet^2 # version 10
rWet2 <- ifel(is.na(rWet2), 0.01, rWet2)  # version 10
#global(rWet2[[1]], fun="isNA")

cost.All <- raster::stack(rWet2)  # Original raster stack (no jenks)
#cost.All <- raster::stack(rWetRcl)  # Jenks reclassified raster stack


#-------------------------------------
### Create a transition layer

# Transition layers are constructed from a raster, and they take the geographic
# references (projection, resolution, extent) from the original raster object.
# They also contain a matrix of probability of movement between cells which can
# be interpreted as conductance

# Create a transition object based on 'cost.All' (a conductance layer)
# Connections here are set based on a 16 neighbor rule (could be set to 4 or 8)

#----------------------------------------
### Correct for geometric distortion

# Transition values are calculated based on values of adjacent cells in the cost
# raster. In lat-long projection, cell sizes become smaller as you move poleward

# Values of the matrix need to be corrected for the type of distortion for our analysis
# the coordinate ref system is NAD83/ Ontario MNR Lambert. 

 # Create transition object and geo-correct in one loop
tr.cost.AllL <- foreach (i=1:nlayers(cost.All)) %do% {
                
temp1 <- gdistance::transition(cost.All[[i]], transitionFunction = mean, directions = 8)   

gdistance::geoCorrection(temp1, type = "c", multpl=FALSE, scl=FALSE)   

          
                }
                
#tr.cost.AllL <- tr.cost.AllL
         
#length(tr.cost.AllL)
 

#------------------------------------------
# Create species specific shortest path objects

#rm(shPathLAll) 

 #'%dopar%' <- foreach::'%dopar%'
 
shPathLAll <- foreach (h=1:length(SpSitesL), .inorder=TRUE, .packages="foreach") %do% {
               foreach (k=1:length(tr.cost.AllL)) %do% {
                foreach (i=1:length(SpSitesL[[h]][,1])) %do% {
                foreach (j=1:length(SpSitesL[[h]][,1])) %do% {         
                  
                   try(
                   gdistance::shortestPath(tr.cost.AllL[[k]], origin= SpSitesL[[h]][i,], goal=SpSitesL[[h]][j,], output="SpatialLines")
                                         
                   )
                                                    
                         }
         
         }
         }
         }        
     
#class(shPathLAll)
#length(shPathLAll)    # 3 
#length(shPathLAll[[1]])   # 8
# shPathLAll[[1]][[1]][[1]][[7]]
 saveRDS(shPathLAll, paste0(outf1,"shPathLAll.rds"))
shPathLAll <- readRDS(paste0(outf1,"shPathLAll.rds"))


#----------------------------------
# Calculated Euclidean distance
#----------------------------------

distEuclidean2L <- foreach (h=1:length(pts_pjL)) %do% {

as.matrix(terra::distance(vect(pts_pjL[[h]])) )
}


rlabels <- c("wetland_1800", "wetland_1967", "wetland_1982", "wetland_2002","wetland2_1800","wetland2_1967","wetland2_1982","wetland2_2002")


#rm(shPathLAll2)
shPathLAll2 <- foreach (g=1:length(SpSitesL)) %do% {  
                 foreach (h=1:length(shPathLAll[[g]]))  %do% {
                  foreach (i=1:length(shPathLAll[[g]][[h]])) %do% {
                     foreach (j=1:length(shPathLAll[[g]][[h]][[i]])) %do% {

         if (i==j) { 
         data.frame(rlyr=h, rlyrlab=rlabels[h], site_from=i, site_from_id=SpSitesL[[g]][i,1][[1]], site_to=j, site_to_id=SpSitesL[[g]][j,1][[1]], distance=0, euc_distance=distEuclidean2L[[g]][i,j])
             
             }  else {
             
         data.frame(rlyr=h, rlyrlab=rlabels[h], site_from=i, site_from_id=SpSitesL[[g]][i,1][[1]], site_to=j, site_to_id=SpSitesL[[g]][j,1][[1]], distance=if(!inherits(shPathLAll[[g]][[h]][[i]][[j]], "try-error")) {rgeos::gLength(shPathLAll[[g]][[h]][[i]][[j]])} else {NA}, euc_distance=distEuclidean2L[[g]][i,j])
                
             }
         }
         
         }
         }
         }

length(shPathLAll2)
#length(shPathLAll2[[1]])  # 8 lyrs
#length(shPathLAll2[[2]])    # 8 lyrs
#length(shPathLAll2[[3]][[8]])   # 13


#---------------------------------------------------
#rm(SpSitesrbind1)                  
SpSitesrbind1 <- foreach (g=1:length(shPathLAll2)) %do% {
                    foreach (i=1:length(shPathLAll2[[g]])) %do% {
                    
                    rbindL1 <- lapply(shPathLAll2[[g]][[i]], function(x) do.call("rbind", x))
                     temp1 <-  do.call("rbind", rbindL1)
                     temp1$path_id <- paste0(temp1$site_from_id, "to", temp1$site_to_id)
                     temp1$sp <- g
                     
                     as.data.frame(temp1)
                    }
                    }
 
#SpSitesrbind1[[2]][[1]]
#SpSitesrbind1[[3]][[4]]  

#rm(rbindL10)
rbindL10 <-   foreach (i=1:length(SpSitesrbind1)) %do% {
                    
                     temp1 <-  do.call("rbind", SpSitesrbind1[[i]])
                     
                     as.data.frame(temp1)
                    }

                    
class(rbindL10) # list
length(rbindL10)  # 3 species-sites 
#class(rbindL10[[1]]) # data frame
#str(rbindL10[[1]])
#dim(rbindL10[[1]]) # X "from" sites times Y "to" sites  times 8 raster layers
#head(rbindL10[[1]])
#dim(rbindL10[[1]])
#dim(rbindL10[[2]])
#head(rbindL10[[3]])
#dim(rbindL10[[3]])

# checks
#rbindL10[[2]][8] > rbindL10[[2]][7] # Should get all false, Euclidean > Least cost path
#summary(rbindL10[[3]][8] > rbindL10[[3]][7]) # Should get all false

 
#===================================================
# Convert list to data frame
#rm(ShPathDf1)
ShPathDf1 <- do.call("rbind", rbindL10)
#dim(ShPathDf1)
#head(ShPathDf1)
#tail(ShPathDf1)

#saveRDS(ShPathDf1, paste0(outf3,"ShPathDf1_5km_jenks1.rds") )
#saveRDS(ShPathDf1, paste0(outf5,"ShPathDf1_1km_jenks1.rds") )

saveRDS(ShPathDf1, paste0(outf2, "ShPathDf1", suffix1, ".rds")  )




#=========================
# References
#=========================

# - https://bookdown.org/hhwagner1/LandGenCourse_book/WE_10.html#WE_10
# - https://cran.r-project.org/web/packages/sf/vignettes/sf2.html
# - https://stackoverflow.com/questions/47815996/combine-data-frames-from-a-nested-list