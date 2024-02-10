#=============================================================
# Create Genetic Model Data Frame - Take II
#=============================================================

# Turtle Landscape Genetics Project  (Naimh's)
# 2024-02-10
# Code authors: Niamh W. & Peter R.
# P. James and MJ Fortin Labs

# Notes:
# 1) Given that we now have several new metrics and ways of creating them, here we show code to put all parts into a whole.
# 2) We will need several objects
#   - df is the object whihc has shortest path metrics
#   - landMdf3 is the objects with Euclidean path buffer (5 km) land cover metrics
#   - shpathDF is the object with shortest path buffer (1 km) land cover metrics
#   - elevDf has elevation, slope, and roughness metrics

# Shortest Path using historic wetlands linked to genetic distances & Euclidean Path Buffers


#===============================
# Libraries
#===============================

library(sf)
library(terra) # Install package terra before "raster" to avoid conflicts
library(foreach)
library(sqldf)


#=================================
# Set folder & files paths
#=================================

fpath3 <- "~/PhD/niamh/data/genetic_distance/"


# Read objects created in previous runs of the R code 
ShPathDf1 <- readRDS("C:/Users/Peter R/Documents/PhD/niamh/output/version8/data/ShPathDf1_1km.rds")
ShPathLcSumDf1 <- readRDS("C:/Users/Peter R/Documents/PhD/niamh/output/version10/data/ShPathLcSumDf1_1km.rds")
costDistDf1 <- readRDS("C:/Users/Peter R/Documents/PhD/niamh/output/version10/data/costDistDf1_1km.rds")
cummDistDf1 <- readRDS("C:/Users/Peter R/Documents/PhD/niamh/output/version10/data/cummDistDf1_1km.rds")


#--------------------------------------------------------------
# THis is a hack as I did not save the Euclidean buffer object
# I should run the Euclidean buff part again and save the object

# Hack starts here!
euPathDf1_bltu <-  readRDS("C:/Users/Peter R/Documents/PhD/niamh/output/version10/data/ShPathGenDistL50_bltu_1km.rds")

euPathDf1_sntu <-  readRDS("C:/Users/Peter R/Documents/PhD/niamh/output/version10/data/ShPathGenDistL50_sntu_1km.rds")

euPathDf1_sptu <-  readRDS("C:/Users/Peter R/Documents/PhD/niamh/output/version10/data/ShPathGenDistL50_sptu_1km.rds")

class(euPathDf1_bltu)
head(euPathDf1_bltu[[1]])

# Percent land cover for raster 4 & 8 only. (For some reason it was taking a long time to do all lyrs on DRAC.)
shPathBuffPerLc <- readRDS(paste0(outf1,prefix1, suffix1,"olcc", "_landMdf3",".rds"))
#class(shPathBuffPerLc)
#dim(shPathBuffPerLc)
#head(shPathBuffPerLc)

elevDf <- readRDS(paste0(outf2, "elevDf_", suffix1, ".rds") )

# Back engineer Euclidean path buffers

euPart1 <- foreach (1:length(euPathDf1_bltu), .inorder=TRUE, .packages = "foreach") %do% {
     
                      temp1 <- euPathDf1_bltu[[i]]
                      temp2 <- temp1[, c(1:2, 9:10, 17:24)] 
         
         }
         
euPart1 <- foreach (1:length(euPathDf1_sntu), .inorder=TRUE, .packages = "foreach") %do% {
     
                      temp1 <- euPathDf1_sntu[[i]]
                      temp2 <- temp1[, c(1:2, 9:10, 17:24)] 
         
         }
         
euPart1 <- foreach (1:length(euPathDf1_sptu), .inorder=TRUE, .packages = "foreach") %do% {
     
                      temp1 <- euPathDf1_sptu[[i]]
                      temp2 <- temp1[, c(1:2, 9:10, 17:24)] 
         
         }

# sptu: [1] 800   14 ; bltu: [2] 968   14; sntu:  [1] 1352 14

class(euPart1[[1]])
head(euPart1[[8]])

euPartDf <- do.call("rbind", euPart1 )
dim(euPartDf)
unique(euPartDf$path_id)

euPathBuff <- list()
euPathBuff[[1]] <- euPartDf 
euPathBuff[[2]] <- euPartDf 
euPathBuff[[3]] <- euPartDf
length(euPathBuff) 

euPartDf2 <- do.call("rbind", euPathBuff)
dim(euPartDf2)
head(euPartDf2)

euPartDf3 <- euPartDf2[, c(3, 5:12)]
euPartDf4 <- sqldf("SELECT distinct * from euPartDf3")
dim(euPartDf4)
head(euPartDf4)

head(euPartDf4[euPartDf4$path_id=='LH1toLE1', ])
head(euPartDf4[euPartDf4$path_id=='LE1toLH1', ])

saveRDS(euPartDf4, paste0(outf2, "ShPathGenDistL500_", suffix1, ".rds") )

# Hack ends here!
#------------------------------------------------
 
 
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

#length(genDistL1)
#head(genDistL1[[1]])
#head(genDistL1[[3]])
#str(genDistL1[[1]])
 

#=======================================
# Joining the objects together
#=======================================

# With cost distances
 ShPathGenDistL10 <-  foreach (i=1:length(genDistL1)) %do% {
                               
                               genDist <- genDistL1[[i]]
                                
                             sqldf(paste0("SELECT t1.*, t2.mean, t2.median, t3.fst, t3.joost, t4.cost_dis, t5.cumm_dis FROM ShPathDf1 t1 JOIN ShPathLcSumDf1 t2 ON t1.path_id=t2.path_id AND t1.rlyr=t2.rlyr AND t1.sp=t2.sp JOIN genDist t3 ON t1.path_id=t3.path_id AND t1.sp=t3.sp JOIN costDistDf1 t4 ON t1.path_id=t4.path_id AND t1.rlyr=t4.rlyr AND t1.sp=t4.sp JOIN cummDistDf1 t5 ON t1.path_id=t5.path_id AND t1.rlyr=t5.rlyr AND t1.sp=t5.sp") )
                                    }
                                    
#head(ShPathGenDistL10[[1]]) 


# Add Euclidean and shortest path buffer metrics and elevation

ShPathGenDistL500 <- foreach (i=1:3)  %do% {

 df1 <- ShPathGenDistL10[[i]]

 sqldf("SELECT t1.*, t2.LC1, t2.LC2, t2.LC3, t2.LC4, t2.LC5, t2.LC6, t2.LC7, t2.LC8,
       t3.LC1 as LC1_spath, t3.LC2 as LC2_spath, t3.LC3 as LC3_spath, 
       t3.LC4 as LC4_spath, t3.LC5 as LC5_spath, t3.LC6 as LC6_spath, t3.LC7 as LC7_spath, t3.LC8 as LC8_spath,
       t4.elevation, t4.slope, t4.roughness
FROM df1 t1 LEFT JOIN  euPartDf4 t2 ON t1.path_id=t2.path_id
LEFT JOIN
shPathBuffPerLc t3 ON t1.rlyr=t3.rlyr AND t1.path_id=t3.path_id
LEFT JOIN
elevDf t4 ON t1.rlyr=t4.rlyr AND t1.path_id=t4.path_id
"
)

}

str(ShPathGenDistL500)
dim(ShPathGenDistL500[[3]])
head(ShPathGenDistL500[[1]])


#=================================================
# Save R objects
#=================================================

# These objects have the buffer euclidean path OLCC metrics (percent lc)
splabels <- c("sptu", "bltu", "sntu")

foreach (i=1:length(splabels)) %do% {

saveRDS(ShPathGenDistL500[[i]], paste0(outf2, "ShPathGenDistL500_", splabels[i], "_", suffix1, ".rds"))
         
         
         }

