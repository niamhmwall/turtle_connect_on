#=========================================================
# CREATE Shortest Path buffers
#=========================================================


# 2024-02-06

# Code authors: Niamh W. & Peter R.
# P. James and MJ Fortin Labs

# Here I used version 10 ShPath objects to create a 1 km buffer and estimate land cover metrics

# Script gdistance_metrics_data_frame_v5 needs to be run first

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
outf1 <- "~/PhD/niamh/output/version10/gis/"  # 
outf2 <- "~/PhD/niamh/output/version10/data/"  # 

shPathLAll <- readRDS(paste0(outf1,"shPathLAll.rds"))

class(shPathLAll[[1]][[1]][[1]][[15]])
dev.new(); plot(shPathLAll[[1]][[1]][[1]][[1]])

terra::vect(shPathLAll[[1]][[1]][[1]][[15]])

shPathSpDf <- foreach (g=1:length(SpSitesL)) %do% {  
                 foreach (h=1:length(shPathLAll[[g]]))  %do% {
                  foreach (i=1:length(shPathLAll[[g]][[h]])) %do% {
                     foreach (j=1:length(shPathLAll[[g]][[h]][[i]])) %do% {


             
   temp1 <- data.frame(rlyr=h, rlyrlab=rlabels[h], site_from=i, site_from_id=SpSitesL[[g]][i,1][[1]], site_to=j, site_to_id=SpSitesL[[g]][j,1][[1]])
   
   temp2 <- vect(shPathLAll[[g]][[h]][[i]][[j]])
   temp2$site_from <- i
   temp2$site_to <- j
        
   terra::merge(temp2, temp1, by = intersect(names(temp2), names(temp1)) )
                
            
         }
         
         }
         }
         }


dev.new(); plot(vect(shPathSpDf[[1]][[1]][[15]]))


shPathSpDf2 <- foreach (g=1:length(shPathSpDf)) %do% {
                    foreach (i=1:length(shPathSpDf[[g]])) %do% {
                    
                    rbindL1 <- lapply(shPathSpDf[[g]][[i]], function(x) do.call("rbind", x))
                     temp1 <-  do.call("rbind", rbindL1)
                     temp1$path_id <- paste0(temp1$site_from_id, "to", temp1$site_to_id)
                     temp1$sp <- g
                     
                     temp1
                    }
                    }


shPathSpDfL10 <-   foreach (i=1:length(shPathSpDf2 )) %do% {
                    
                     temp1 <-  do.call("rbind", shPathSpDf2[[i]])
                     
                     temp1
                     
                     #as.data.frame(temp1)
                    }
                    

ShPathSpDf10 <- do.call("rbind", shPathSpDfL10 )

class(ShPathSpDf10)
dim(ShPathSpDf10)
head(ShPathSpDf10)

# Remove empty lines and duplicates

ShPathSpDf11 <- ShPathSpDf10[ShPathSpDf10$site_from_id!=ShPathSpDf10$site_to_id, 1:8]
dim(ShPathSpDf11)
ShPathSpDf11$rid <- 1:nrow(ShPathSpDf11)
head(ShPathSpDf11)
class(ShPathSpDf11)

#rm(ShPathSpDf12)

terra::writeVector(ShPathSpDf11, paste0("tur_shpath_v1.shp"), filetype = "ESRI Shapefile", overwrite=TRUE)


 #===========================
# Read object from PG
#===========================

# Note: I created some Views in Postgres as it was easier to do so than with terra or sf

# Create a connection to the database
# add user and password
con1 <- dbConnect(RPostgres::Postgres(), user="postgres", password="Passpost123",
                 host="localhost", port=5432, dbname="resnet1")

# This does nott set up the project or spatial index. Better to use PostGIS Shp loader                
#st_write(obj = st_as_sf(ShPathSpDf11), dsn = con1, Id(schema="public", table = "tur_shpath_v1") )

# create buffer in Postgis and load here
                 

 # only table name is needed
query1 <- "tur_shpath_buffers_1km_v1"

bufferShPath <- st_read(con1, query1)
str(bufferShPath)


terra::writeVector(vect(bufferShPath), "/tur_shpath_1km_buffer_v1.shp", filetype = "ESRI Shapefile", overwrite=TRUE)


g1 <- terra::vect(bufferShPath)

grids1Pj <- st_transform(sf::st_as_sf(g1), crs(r3))

landM_shPath <- foreach(i=1:nlyr(rwetL40)) %do%
                  sample_lsm(raster::raster(rwetL40[[i]]), grids1Pj, plot_id=grids1Pj$"path_id", 
                      what = c("lsm_c_pland", "lsm_c_te", "lsm_l_np", "lsm_l_pd", "lsm_l_ta"))
