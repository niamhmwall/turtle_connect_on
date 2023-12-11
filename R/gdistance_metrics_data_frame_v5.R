#=====================================================
# Create Gdistance Data Frame - Niamh's Turtle Project
#=====================================================

# 2023-12-11
# Code authors: Niamh W. & Peter R.

# Aim: Produce gdistance stats using historic wetland data


# Tutorial:
#    https://bookdown.org/hhwagner1/LandGenCourse_book/WE_10.html#WE_10

# sessionInfo()
# [1] gdistance_1.6.2 Matrix_1.5-4    igraph_1.4.2    raster_3.6-20   sp_1.4-5       
# [6] foreach_1.5.1   terra_1.7-23    sf_1.0-12 


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


#=================================
# Set folder & files paths
#=================================

# Set wording directory
setwd("C:/Users/Peter R/Documents/PhD/niamh/")
setwd("H:/niamh/scripts/version2/")
# Niamh's
#setwd()

# Peter's data path
#fpath <- "C:/Users/Peter R/Documents/PhD/niamh/output/version3/gis"
# Niamh's'
#fpath <- "C:/Users/Niamh/Documents/PhD/niamh/output/version2/gis"
# Project data path
fpath <- "../../output/version3/gis"

fpath2 <- "../../misc"


# Output folder for rasters

outf0 <- "../../output/version2/gis/"
outf1 <- "../../output/version4/gis/"

# Output folder for R figs
outf2 <- "../../output/version3/img/"

outf3 <- "../../output/version3/data/"


#===================================
# Rasterize Land cover percentages 
#===================================

# Note: the tifs & shp rasterized below are outputs of lcover_metrics_v3.R script.

 # list tif files. 
#rastfiles <- list.files(path=fpath, pattern = "*.tif", full.names = TRUE) # This loads 6 rasters in my folder


# Original 10 km 
rastfiles2 <- list.files(path=outf0, pattern = "wetland_.*per_lc_10km.tif$", full.names = TRUE) #

 
# 5 km 
rastfiles2 <- list.files(path=outf1, pattern = "wetland_.*per_lc_5km.tif$", full.names = TRUE) #

# 1 km augmented
#rastfiles2 <- list.files(path=outf1, pattern = "wetland.*per_lc_1km.tif$", full.names = TRUE) #

#s1 <- raster::stack(rastfiles2[2])   # Note: OLCC has different ext, crs, etc. [2] has a correct tif.
#class(s1)

# Change the resolution to "1000, 1000" (10km grid) using terra's aggregate method
 # Create 10 km by 10 km raster template to rasterize polygons (polygons were created earlier using the landscape metric R package). We only use the first raster layer.
#r1Template <- terra::aggregate(rast(s1[[1]]), 100, fun="mean", na.rm=T)  # 10 km res
 #coord. ref. : NAD83 / Ontario MNR Lambert (EPSG:3161) 


# Now we crate a new raster stack to run gdistance.
#rWet <- c(rWet1800, rWet1967, rWet1982)  # Create a terra raster stack
rWet <- rast(rastfiles2)
nlyr(rWet)

#----------------------------------------------------------------------
# Reclassify wetland raster using natural breaks (jenks, fisher-jenks)

val1 <- terra::values(rWet[[1]])
length(val1)
hist(val1)
summary(val1)

# Get class interval using Jenks/Fisher-Jenks (i.e., natural breaks)
# Fisher-Jenks runs faster for n>3000. Note that when n is large a 10% sample is used.
# Hence, the breaks are not equal each time you run classIntervals()

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
# 5 km crete breaks manually using  object val1Rcl$brks. 
m <- c(0, 20.8, 1000, 20.8, 54.8, 100, 54.8, 100, 10)

# 10 km
#val1Rcl$brks
#[1]  0.01981768 20.92833424 52.62652206 98.97857666
m <- c(0, 20.9, 1000, 20.9, 52.6, 100, 52.6, 100, 10)

rclM1 <- matrix(m, ncol=3, byrow=TRUE)

# Reclassify raster, all layers at once
rWetRcl <- classify(rWet, rclM1, include.lowest=FALSE, right=TRUE)
#freq(rWetRcl[[1]])
freq(rWetRcl)
#freq(rWetRcl[[8]])

plot(rWetRcl[[1]])
dev.new(); plot(rWetRcl[[4]])


#===================================
# Load sites (points) 
#===================================


#pts <- sf::st_read(paste0(fpath2, "/Site_samplingloc_June23.csv"), options=c("X_POSSIBLE_NAMES=Longitude","Y_POSSIBLE_NAMES=Latitude"), crs=4326)

pts <- sf::st_read(paste0(fpath2, "/Site_samplingloc_Oct20_2023.csv"), options=c("X_POSSIBLE_NAMES=Longitude","Y_POSSIBLE_NAMES=Latitude"), crs=4326)

#pts <- sf::st_read(paste0(fpath2, "/Site_samplingloc_5km_Dec9_2023.csv"), options=c("X_POSSIBLE_NAMES=Longitude","Y_POSSIBLE_NAMES=Latitude"), crs=4326)


# create list with points
pts_pjL <- list()

# 1) SPTU
# Leave some sites out
pts_pjL[[1]] <- sf::st_transform(pts[!(pts$Site %in% c('EL', 'GB1','GB2','GB3','GB4', 'BC','BC1', 'ALG', 'HU')), ], crs(rWet))


# 2) BLTU
# here we write sites to include
# 'LH1', 'HC', 'EO1', 'EO', 'ALG', 'HU', 'GB4', 'GB3', 'LE3', 'BC1', 'LE1'
# Removed
# 'ALG', 'HU', 'GB4', 'GB3', 'LE3', 'BC1' 
# I fixed the code so now I no longer need to remove gap sites. Not quite
pts_pjL[[2]] <- sf::st_transform(pts[(pts$Site %in% c('LH1','HC', 'EO1','EO', 'ALG', 'HU', 'GB4', 'GB3', 'LE3', 'BC1', 'LE1')),], crs(rWet))

# 3) SNTU
# here we write sites to include
#'LH1', 'BP', 'LE1', 'HC', 'LH2', 'GB2', 'EO', 'BC', 'GH', 'KAW', 'N', 'ALG', 'HU'
# removed
#'GB2', 'GH', 'BC', 'GH', 'N', 'ALG', 'HU', 
# I fixed the code so now I no longer need to remove gap sites 
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

# Note: rasters used below need to have variation. They can't be a single land cover value.

# We have a raster stack (terra)
#class(s1)
#class(rWet)
class(rWetRcl)

rWet2 <- 100-rWet

cost.All <- raster::stack(rWet2)  # raster package can read SpatRast 

cost.All <- raster::stack(rWetRcl)  # raster package can read SpatRast 

#-------------------------------------
### Create a transition layer

# Transition layers are constructed from a raster, and they take the geographic
# references (projection, resolution, extent) from the original raster object.
# They also contain a matrix of probability of movement between cells which can
# be interpreted as conductance

# Create a transition object based on 'cost.1800' (a conductance layer)
# Connections here are set based on a 16 neighbor rule (could be set to 4 or 8)

#----------------------------------------
### Correct for geometric distortion

# Transition values are calculated based on values of adjacent cells in the cost
# raster. In lat-long projection, cell sizes become smaller as you move poleward

# Values of the matrix need to be corrected for the type of distortion for our analysis
# the coordinate ref system is NAD83/ Ontario MNR Lambert (which I think is comparable to
# lat-long but have to double check this). Might need to add the second correction

 # Create transition object and geo-correct in one loop
tr.cost.AllL <- foreach (i=1:nlayers(cost.All)) %do% {
                
temp1 <- gdistance::transition(cost.All[[i]], transitionFunction = mean, directions = 8)   

gdistance::geoCorrection(temp1, type = "c", multpl=FALSE, scl=FALSE)             
                }
                
#tr.cost.AllL_10km <- tr.cost.AllL
         
#length(tr.cost.AllL)
 

#-----------------------------------------
#Plot shortest path in space

# Plot site locations on top of conductance raster 'tr.cost.1800'
# Calculate the least cost path between sites LH1 and LE2 as a trial
# Plot the shortest path on a map


#------------------------------------------
# Create species specific shortest path objects

rm(shPathLAll) 
shPathLAll <- foreach (h=1:length(SpSitesL)) %do% {
               foreach (k=1:length(tr.cost.AllL)) %do% {
                foreach (i=1:length(SpSitesL[[h]][,1])) %do% {
                foreach (j=1:length(SpSitesL[[h]][,1]))  %do% {         
                  
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
# head(shPathLAll[[1]][[1]][[1]][[7]])       


#----------------------------------
# Calculated Euclidead distance
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
#length(shPathLAll2[[3]][[4]])   # 8 lyrs


#---------------------------------------------------
rm(SpSitesrbind1)                  
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
#SpSitesrbind1[[3]][[4]]  # It works! Add all gap sites tomorrow.

rm(rbindL10)
rbindL10 <-   foreach (i=1:length(SpSitesrbind1)) %do% {
                    
                     temp1 <-  do.call("rbind", SpSitesrbind1[[i]])
                     
                     as.data.frame(temp1)
                    }

                    
class(rbindL10) # list
length(rbindL10)  # 3 species-sites 
class(rbindL10[[1]]) # data frame
str(rbindL10[[1]])
dim(rbindL10[[1]]) # X "from" sites times Y "to" sites  times 8 raster layers
head(rbindL10[[1]])
dim(rbindL10[[1]])
dim(rbindL10[[2]])
head(rbindL10[[3]])
dim(rbindL10[[3]])

# checks
#rbindL10[[2]][8] > rbindL10[[2]][7] # Should get all false
#rbindL10[[3]][8] > rbindL10[[3]][7] # Should get all false

#write.csv(rbindL10[[1]], paste0(outf1,"test_path_df.csv"))

#write.table(format(rbindL10[[1]][, c(2, 9,7,8)]), 
#            paste0(outf1, "test_path_df.csv"), row.names=F, 
#            sep=',', quote = F)



 #shPathLAll_5km <- shPathLAll
 
# shPathLAll_10km <- shPathLAll
 
#------------------------------------------- 
# Plots                                   
#raster::plot(raster::raster(tr.cost.AllL[[1]]), xlab="x coordinate (m)", ylab="y coordinate (m)", legend.lab="Conductance", main="Wetland 1800")
##lines(LE2toLH1, col="red", lwd=2) #PR
#lines(shPathLAll[[1]][[1]][[1]][[4]], col="red", lwd=2 )
#lines(shPathLAll_5km[[1]][[1]][[1]][[4]], col="black", lwd=2 )  # 5km
##lines(LH1toLE1, col="red", lwd=2 )
##lines(shPathL1[[1]][[3]], col="red", lwd=2 ) # Plot object in loop
##points(pts_pj[1:2,])
##plot(sites[1:2,], add=TRUE)
#plot(SpSitesL[[1]][c(1,4),], add=TRUE)
#plot(sites, add=TRUE)

#pts_pj[1,]


#===================================================
# Convert list to data frame
rm(ShPathDf1)
ShPathDf1 <- do.call("rbind", rbindL10)
dim(ShPathDf1)
head(ShPathDf1)
tail(ShPathDf1)

saveRDS(ShPathDf1, paste0(outf3,"ShPathDf3_10km.rds") )

#---------------------------------------------
# Skipped for not as it no longer seems useful
# Restructure the data from long to wide format

#ShPathDf1Wide <- dcast(ShPathDf1[,c(9,2, 7,8)], path_id + euc_distance  ~ rlyrlab, value.var="distance")
#dim(ShPathDf1Wide)
#head(ShPathDf1Wide)
#tail(ShPathDf1Wide)
##pathDf1Wide[pathDf1Wide$path_id=="LH1toHC",]
#
#saveRDS(ShPathDf1Wide, paste0(outf3,"ShPathDf1Wide.rds") )

#---------------------------------------------


#=========================
# References
#=========================

# - https://bookdown.org/hhwagner1/LandGenCourse_book/WE_10.html#WE_10
# - https://cran.r-project.org/web/packages/sf/vignettes/sf2.html
# - https://stackoverflow.com/questions/47815996/combine-data-frames-from-a-nested-list