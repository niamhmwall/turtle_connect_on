#=================================================
# Add Land cover metrics to Shortest Path 
#=================================================

# Turtle Landscape Genetics Project  (Naimh's)
# 2023-12-14
# Code authors: Niamh W. & Peter R.
# P. James and MJ Fortin Labs

# Aim: Produce cummuative sum stats using shortest patha  and historic wetland data.


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

library(plyr)
library(reshape2)

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
fpath <- "~/PhD/niamh/output/version3/gis/"
# Niamh's'
#fpath <- "C:/Users/Niamh/Documents/PhD/niamh/output/version2/gis"

fpath2 <- "~/PhD/niamh/misc/"
 

# Output folder for rasters
#outf0 <- "~/PhD/niamh/output/version2/gis/"  # For spatial data, 10 km
#outf1 <- "~/PhD/niamh/output/version3/gis/"  # For spatial data, 5 km
#outf2 <- "~/PhD/niamh/output/version3/img/"  # For images, figures
#outf3 <- "~/PhD/niamh/output/version3/data/" # For non-spatial data
#outf4 <- "~/PhD/niamh/output/version4/gis/"  # For spatial data, X km, jenks
#outf5 <- "~/PhD/niamh/output/version4/data/"  # For spatial data, X km, jenks
#outf1 <- "~/PhD/niamh/output/version5/gis/"  # 
#outf2 <- "~/PhD/niamh/output/version5/data/"  # 

# These prefix and suffixes are need to create files with the correct labels
#prefix1 <- "utm_grid_10km_"
#prefix1 <- "utm_grid_5km_"
#prefix1 <- "utm_grid_1km_"

#suffix2 <- "_10km_jenks1"
#suffix2 <- "_5km_jenks1"
#suffix2 <- "_1km_jenks1"
suffix1 <- "_1km"
#suffix1 <- "_1km_jenks1"

#===================================
# Read data
#===================================
# Notes:
# - You need to run gdistance_metrics_data_frame_v5.R first so that the object rWet is created
# - You may want to to cummulative sum of rWetRcl instead.

#==========================================================================       
# Create list with values of all raster cells transversed by leastCostPath. 
#==========================================================================

# Object shPathLAll is created with gdistance_metrics_data_frame_v5.R, run it first
 
#rm(shPathlcL1)
 shPathlcL1 <- foreach (g=1:length(shPathLAll)) %do% {
                foreach (k=1:length(shPathLAll[[g]])) %do% {

                extCellsFromSitesL1 <- foreach (i=1:length(SpSitesL[[g]][,1])) %do% {

                extCellsL1 <- foreach (j=1:length(SpSitesL[[g]][,1]))  %do% {         
                    

                    
                    if (!inherits(shPathLAll[[g]][[k]][[i]][[j]], "try-error")) {

                    lineshPath1 <- shPathLAll[[g]][[k]][[i]][[j]]
                    lineshPath2 <- terra::vect(lineshPath1)  
                     
                        terra::extract(rWet[[k]], lineshPath2)
                        
                        }  else {
                        
                           data.frame(ID=c(0), sum=c(NA))
                        
                        }
                                        
                      }    
                              
         }       
         }  
         }
         
         
 class(shPathlcL1)

rlabels <- c("wetland_1800", "wetland_1967", "wetland_1982", "wetland_2002","wetland2_1800","wetland2_1967","wetland2_1982","wetland2_2002")


# Summary metrics of raster cell values transversed by leastCostPath.

#rm(shPathlcSum)
shPathlcSum <- foreach (g=1:length(shPathlcL1)) %do% {
                foreach (k=1:length(shPathlcL1[[g]])) %do% { 
                   foreach (i=1:length(SpSitesL[[g]][,1]))  %do% { 
                           foreach (j=1:length(SpSitesL[[g]][,1]))  %do% {
                           cbind.data.frame("rlyr"= k, rlyrlab= rlabels[k], site_from=i, site_from_id=SpSitesL[[g]][i,1][[1]], site_to=j, site_to_id=SpSitesL[[g]][j,1][[1]], "mean"=mean(shPathlcL1[[g]][[k]][[i]][[j]][,2], na.rm=T),
                  "median"=median(shPathlcL1[[g]][[k]][[i]][[j]][,2], na.rm=T))
                  }
    }
    }
    }

length(shPathlcSum)

class(shPathlcSum[[1]])

#  
                    
#rm(rbindPathlcSum1)                    
rbindPathlcSum1 <- foreach (g=1:length(shPathlcSum)) %do% {
                    foreach (k=1:length(shPathlcSum[[g]])) %do% {
                    
                    rbindL1 <- lapply(shPathlcSum[[g]][[k]], function(x) do.call("rbind", x))
                     temp1 <-  as.data.frame(do.call("rbind", rbindL1))
                     temp1$path_id <- paste0(temp1$site_from_id, "to", temp1$site_to_id)
                     temp1$sp <- g
                     
                     as.data.frame(temp1)
                    }   
                    } 
                    

rbindPathlcSum2 <-   foreach (i=1:length(rbindPathlcSum1)) %do% {
                    
                     temp1 <-  do.call("rbind", rbindPathlcSum1[[i]])
                     
                     as.data.frame(temp1)
                    }                         

class(rbindPathlcSum2[[1]])
#(rbindPathlcSum2[[1]])

# long data frame with summary stats of raster cells traversed by leastCostPath
#ShPathLcSumDf1 <- do.call("rbind", rbindPathlcSum1)
#rm(ShPathLcSumDf1)
ShPathLcSumDf1 <- do.call("rbind", rbindPathlcSum2)

#class(ShPathLcSumDf1)
#str(ShPathLcSumDf1)
                                 
#saveRDS(ShPathLcSumDf1, paste0(outf3,"ShPathLcSumDf1_5km_jenks1.rds") )
#saveRDS(ShPathLcSumDf1, paste0(outf5,"ShPathLcSumDf1_1km_jenks1.rds") )

saveRDS(ShPathLcSumDf1, paste0(outf2, "ShPathLcSumDf1", suffix1, ".rds")  )

#head(ShPathLcSumDf1)

#ShPathLcSumDf1[ShPathLcSumDf1$rlyr==2,]  # NaN, NA

 
