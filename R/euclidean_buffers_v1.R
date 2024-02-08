#=========================================================
# CREATE Euclidean buffers
#=========================================================

library(DBI)

# Number of total linestrings to be created
#n <- nrow(pts) - 1
#
## Build linestrings
#linestrings2 <- lapply(X = 1:n, FUN = function(x) {
#
#  pair <- st_union(pts[x,c(1,4)], pts[x, 1])
#  line <- st_cast(pair, "LINESTRING")
#  return(line)
#
#})
#
#length(linestrings2)

#st_cast(st_union(pts[x,], pts[x+1, 1]), "LINESTRING")
  
  #plot(st_cast(st_union(pts[1,], pts[2, 1]), "LINESTRING"))
  
  
# pts contains sites  
lineEuclideanL <-  foreach (i=1:nrow(pts[,1]), .combine='c') %do% {
                foreach (j=1:nrow(pts[,1])) %do% { 
                         
                         if (i!=j)        
                  
                   #try(
                  pair <- sf::st_union(pts[i, 1], pts[j,1])
                  line1 <- sf::st_cast(pair, "LINESTRING")             
                  
                                         
                   #)
                                                    
                         }
         
         }
         
length(lineEuclideanL)
class(lineEuclideanL[[1]])

lineEuclidean <- do.call("rbind", lineEuclideanL)
lineEuclidean <- unique(lineEuclidean)
dim(lineEuclidean)
head(lineEuclidean)

#23*(23-1)/2   # 253

#library(dplyr)

#lines_distinct <- data.frame(st_geometry(lineEuclidean)) %>% group_by_all('Site') 
#dim(lines_distinct)
#head(lines_distinct)


lineEuclidean$id <- 1:nrow(lineEuclidean)

lineEuclidean5 <- lineEuclidean[,3]
class(lineEuclidean5)
lineEuclidean6 <- vect(lineEuclidean5)
lineEuclidean6 <- unique(lineEuclidean6)
bufferEuclidean2 <- terra::buffer(lineEuclidean6, width=5000, quadsegs=10)
bufferEuclidean2$rid <-  1:nrow(bufferEuclidean2)
crs(bufferEuclidean2) <- "epsg:4326"
plot(bufferEuclidean2)


terra::writeVector(bufferEuclidean2, "tur_sites_10km_buffer_v1.shp", filetype = "ESRI Shapefile")
terra::writeVector(vect(lineEuclidean), "tur_sites_lines_v1.shp", filetype = "ESRI Shapefile")

# Using PostGIS loader I imported the shp above to Postgres (PG)


# Spatial join
#relate(vect(pts[1,]), bufferEuclidean2, "intersects")



#===========================
# Read object from PG
#===========================

# Note: I created some Views in Postgres as it was easier to do so than with terra or sf

# Create a connection to the database
# add user and password
con1 <- dbConnect(RPostgres::Postgres(), user="postgres", password="Passpost123",
                 host="localhost", port=5432, dbname="resnet1")

 # only table name is needed
query1 <- "tur_unique_site_pair_buffers_v1"

bufferEuc <- st_read(con1, query1)
str(bufferEuc)

terra::writeVector(vect(bufferEuc), "tur_sites_5km_buffer_v2.shp", filetype = "ESRI Shapefile", overwrite=TRUE)
#terra::writeVector(vect(lineEuclidean), "tur_sites_lines_v1.shp", filetype = "ESRI Shapefile")





