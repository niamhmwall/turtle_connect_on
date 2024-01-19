#=============================================================
# Create Genetic Model Data Frame - Niamh's Turtle Project
#=============================================================

# 2023-12-15
# Code authors: Niamh W. & Peter R.
# P. James and MJ Fortin Labs

# Aim: 
# - Produce a data frame to be the input for genetic models. 
# - This dataframe consolidates the output of the other these R scripts & CSVs:
#    - gdistance_metrics_data_frame_v5.R
#    - shortest_path_with_lc_metrics_v3.R
#    - Xtu.Fst.csv, Xtu.Joost.csv 

    
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

library(sqldf)

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
#setwd("~/PhD/niamh/")
# Niamh's
#setwd()

# Peter's data path
#fpath <- "~/PhD/niamh/output/version3/gis/"
# Niamh's'
#fpath <- "C:/Users/Niamh/Documents/PhD/niamh/output/version2/gis"

fpath2 <- "~/PhD/niamh/misc/"

fpath3 <- "~/PhD/niamh/data/genetic_distance/"

# Output folder for rasters
outf0 <- "~/PhD/niamh/output/version2/gis/"  # For spatial data, 10 km
#outf1 <- "~/PhD/niamh/output/version3/gis/"  # For spatial data, 5 km
#outf2 <- "~/PhD/niamh/output/version3/img/"  # For images, figures
#outf3 <- "~/PhD/niamh/output/version3/data/" # For non-spatial data
#outf4 <- "~/PhD/niamh/output/version4/gis/"  # For spatial data, X km, jenks
#outf5 <- "~/PhD/niamh/output/version4/data/"  # For spatial data, X km, jenks
outf1 <- "~/PhD/niamh/output/version5/gis/"  # 
outf2 <- "~/PhD/niamh/output/version5/data/"  # 


# These prefix and suffixes are need to create files with the correct labels
#prefix1 <- "utm_grid_10km_"
#prefix1 <- "utm_grid_5km_"
#prefix1 <- "utm_grid_1km_"

#suffix2 <- "_10km_jenks1"
#suffix2 <- "_5km_jenks1"
#suffix2 <- "_1km_jenks1"

suffix1 <- "_1km"
#suffix1 <- "_1km_jenks1"


#===============================
# Read genetic data
#===============================

#-------------------------------
# FST
fstL1 <- list()

fstL1[[1]] <- read.csv(paste0(fpath3,"sptu.Fst.csv"))
fstL1[[1]]$path_id <- paste0(fstL1[[1]]$From, "to", fstL1[[1]]$To)

fstL1[[2]] <- read.csv(paste0(fpath3,"bltu.Fst.csv"))
fstL1[[2]]$path_id <- paste0(fstL1[[2]]$From, "to", fstL1[[2]]$To)

fstL1[[3]] <- read.csv(paste0(fpath3,"sntu.Fst.csv"))
fstL1[[3]]$path_id <- paste0(fstL1[[3]]$From, "to", fstL1[[3]]$To)


#-------------------------------
# Joost
joostL1<- list()

joostL1[[1]] <- read.csv(paste0(fpath3,"sptu.Joost.csv"))
joostL1[[1]]$path_id <- paste0(joostL1[[1]]$From, "to", joostL1[[1]]$To)

joostL1[[2]] <- read.csv(paste0(fpath3,"sptu.Joost.csv"))
joostL1[[2]]$path_id <- paste0(joostL1[[2]]$From, "to", joostL1[[2]]$To)

joostL1[[3]] <- read.csv(paste0(fpath3,"sptu.Joost.csv"))
joostL1[[3]]$path_id <- paste0(joostL1[[3]]$From, "to", joostL1[[3]]$To)


#-------------------------------
# Join genetic data frames
#rm(genDistL1)
genDistL1 <- foreach (i=1:length(fstL1)) %do% {
         
        fst <- fstL1[[i]] 
        joost <- joostL1[[i]]
          
        sqldf(paste0("SELECT t1.path_id, t1.'From', t1.'To', t1.value as fst, t2.value as joost, ", i ," as sp FROM  fst t1 LEFT JOIN joost t2 ON t1.path_id=t2.path_id"))
          
          }

length(genDistL1)
#head(genDistL1[[1]])
#head(genDistL1[[3]])
#str(genDistL1[[1]])


#=================================================
# Create analytical data frame  for genetic model
#=================================================

# ShPathDf1 is created here: C:/Users/Peter R/Documents/PhD/niamh/scripts/version2/gdistance_metrics_data_frame_v1.R
# 1) run this first  or load object
#source("C:/Users/Peter R/Documents/PhD/niamh/scripts/version2/gdistance_metrics_data_frame_v3.R")
#

## ShPathLcSumDf1 is created here: C:/Users/Peter R/Documents/PhD/niamh/scripts/version2/shortest_path_with_lc_metrics_v1.R
## 2) run this second  or load object
##source("C:/Users/Peter R/Documents/PhD/niamh/scripts/version2/shortest_path_with_lc_metrics_v2.R")
#

## 3) genDist is created here (this file): C:/Users/Peter R/Documents/PhD/niamh/scripts/version2/create_df_for_genetic_model_v1.R
#


#ShPathDf1 <- readRDS( paste0(outf3,"ShPathDf1_5km_jenks1.rds") )
ShPathDf1 <- readRDS( paste0(outf2,"ShPathDf1_1km.rds") )
class(ShPathDf1)
str(ShPathDf1)
summary(ShPathDf1)


#ShPathLcSumDf1 <- readRDS(paste0(outf3,"ShPathLcSumDf1_5km_jenks1.rds") )
ShPathLcSumDf1 <- readRDS(paste0(outf2,"ShPathLcSumDf1_1km.rds") )
class(ShPathLcSumDf1)
str(ShPathLcSumDf1)
summary(ShPathLcSumDf1)


ShPathGenDistL1 <-  foreach (i=1:length(genDistL1)) %do% {
                               
                               genDist <- genDistL1[[i]]
                                
                             sqldf(paste0("SELECT t1.*, t2.mean, t2.median, t3.fst, t3.joost FROM ShPathDf1 t1 JOIN ShPathLcSumDf1 t2 ON t1.path_id=t2.path_id AND t1.rlyr=t2.rlyr AND t1.sp=t2.sp JOIN genDist t3 ON t1.path_id=t3.path_id AND t1.sp=t3.sp") )
                                    }

 
class(ShPathGenDistL1)
str(ShPathGenDistL1)  # 3 species
#summary(ShPathGenDistL1[[3]])  

dim(ShPathGenDistL1[[3]]) # sptu: [1] 800   14 ; bltu: [2] 968   14; sntu:  [1] 1352 14
#head(ShPathGenDistL1[[1]])
#tail(ShPathGenDistL1[[1]])



#---------------------------------------------------
# Create a list to ease model processing/creation

rlabels <- c("wetland_1800", "wetland_1967", "wetland_1982", "wetland_2002","wetland2_1800","wetland2_1967","wetland2_1982","wetland2_2002")


ShPathGenDistL2 <- foreach (i=1:length(ShPathGenDistL1)) %do% {
                        
                        foreach (j=1:length(rlabels)) %do% {
         
                                ShPathGenDistL1[[i]][ShPathGenDistL1[[i]]$rlyrlab==rlabels[j], ]
         
                                              }
         
         }
 

length(ShPathGenDistL2) #8
#class(ShPathGenDistL2[[1]]) 
#length(ShPathGenDistL2[[1]]) 
#class(ShPathGenDistL2[[1]][[1]])
#dim(ShPathGenDistL2[[1]][[1]])
#dim(ShPathGenDistL2[[2]][[1]])
#dim(ShPathGenDistL2[[3]][[1]])
#
#
#head(ShPathGenDistL2[[1]][[1]])
#head(ShPathGenDistL2[[1]][[2]])
#head(ShPathGenDistL2[[1]][[3]])
#head(ShPathGenDistL2[[1]][[4]])
#head(ShPathGenDistL2[[1]][[8]])



#=================================================
# Save R objects
#=================================================

splabels <- c("sptu", "bltu", "sntu")
#reslabel <- "_5km" # change here  to _10km, _5km, or _1km as needed

foreach (i=1:length(splabels)) %do% {

saveRDS(ShPathGenDistL2[[i]], paste0(outf2,"ShPathGenDistL1_", splabels[i], suffix1, ".rds"))
         
         
         }

#saveRDS(ShPathGenDistL2[[1]], paste0(outf3,"ShPathGenDistL1_sptu_5km_jenks_v1.rds"))
#saveRDS(ShPathGenDistL2[[2]], paste0(outf3,"ShPathGenDistL1_bltu_5km_jenks_v1.rds"))
#saveRDS(ShPathGenDistL2[[3]], paste0(outf3,"ShPathGenDistL1_sntu_5km_jenks_v1.rds"))



