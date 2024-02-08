#===========================================================
# Calculate Cummulative distances
#===========================================================

# 2024-01-25

# 2024-01-25
# Niamh Wall & Peter R.
# P. James and MJ Fortin Labs


# Notes:
# 1) Cumulative Distance object needs it own transition object as it uses type="r"

 # Create transition object and geo-correct in one loop

rm(tr.cost.AllL)
tr.cost.AllL <- foreach (i=1:nlayers(cost.All)) %do% {
                
temp1 <- gdistance::transition(cost.All[[i]], transitionFunction = mean, directions = 8)   

gdistance::geoCorrection(temp1, type = "r", multpl=FALSE, scl=TRUE)

         
                }
                


cummDistLAll <- foreach (h=1:length(SpSitesL)) %do% {
               foreach (k=1:length(tr.cost.AllL)) %do% {
                 
                   try(
                   #gdistance::shortestPath(tr.cost.AllL[[k]], origin= SpSitesL[[h]][i,], goal=SpSitesL[[h]][j,], output="SpatialLines")
                   
               temp1 <- gdistance::commuteDistance(tr.cost.AllL[[k]], SpSitesL[[h]])
                         
                   
                   )
                   
    siteNames <- as.data.frame(SpSitesL[[h]])[,"Site"]   
    
    siteNames2 <- rep(siteNames, each=length(siteNames))
                                 
    temp2 <- melt(as.matrix(temp1), varnames = c("row", "col"))  
    
    temp2$rlyr <- k
    temp2$sp <- h
                                  
    temp3 <-  cbind(temp2, siteNames, siteNames2)
                   
    temp3$path_id <- paste0(siteNames, "to", siteNames2)
                   
    names(temp3) <- c("to", "from", "cumm_dis", "rlyr", "sp", "to_id", "from_id", "path_id") 
    
    temp3  
                                                    
                         }
         
         }
 
class(cummDistLAll)        
cummDistLAll[[1]][[8]]
cummDistLAll[[3]][[8]]
#hist(cummDistLAll[[3]][[8]][,3])
#summary(cummDistLAll[[3]][[8]][,3])
#
#
#dev.new(); hist(costDistLAll[[3]][[8]][,3])
 
 


rm(cummDist)
cummDist <- foreach (i=1:length(cummDistLAll)) %do% {
                    
                     temp1 <-  do.call("rbind", cummDistLAll[[i]])
                     
                     as.data.frame(temp1)
                    }

length(cummDist)

cummDistDf1 <- do.call("rbind", cummDist)
#summary(cummDistDf1)

dim(cummDistDf1)
#class(cummDistDf1)
#head(cummDistDf1)
#summary(cummDistDf1)
#names(cummDistDf1) <- c("to", "from", "cumm_dis", "rlyr", "sp", "to_id", "from_id", "path_id") 

 saveRDS(cummDistDf1, paste0(outf2, "cummDistDf1", suffix1, ".rds")  )

