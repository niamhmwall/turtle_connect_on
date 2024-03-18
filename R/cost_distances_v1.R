#===========================================================
# Calculate cost distances
#===========================================================

# 2024-01-23

# 2024-01-23
# Niamh Wall & Peter R.
# P. James and MJ Fortin Labs


# Notes:


rm(costDistLAll)
costDistLAll <- foreach (h=1:length(SpSitesL)) %do% {
               foreach (k=1:length(tr.cost.AllL)) %do% {
                 
                   try(
                   #gdistance::shortestPath(tr.cost.AllL[[k]], origin= SpSitesL[[h]][i,], goal=SpSitesL[[h]][j,], output="SpatialLines")
                   
               temp1 <- gdistance::costDistance(tr.cost.AllL[[k]], SpSitesL[[h]])
                         
                   
                   )
                   
    siteNames <- as.data.frame(SpSitesL[[h]])[,"Site"]   
    
    siteNames2 <- rep(siteNames, each=length(siteNames))
                                 
    temp2 <- melt(as.matrix(temp1), varnames = c("row", "col"))  
    
    temp2$rlyr <- k
    temp2$sp <- h
                                  
    temp3 <-  cbind(temp2, siteNames, siteNames2)
                   
    temp3$path_id <- paste0(siteNames, "to", siteNames2)
                   
    names(temp3) <- c("to", "from", "cost_dis", "rlyr", "sp", "to_id", "from_id", "path_id") 
    
    temp3  
                                                    
                         }
         
         }
 
class(costDistLAll)        
#costDistLAll[[1]][[8]]
#costDistLAll[[3]][[8]]
 
 



costDist <- foreach (i=1:length(costDistLAll)) %do% {
                    
                     temp1 <-  do.call("rbind", costDistLAll[[i]])
                     
                     as.data.frame(temp1)
                    }

length(costDist)

costDistDf1 <- do.call("rbind", costDist)
#summary(costDistDf1)

dim(costDistDf1)

saveRDS(costDistDf1, paste0(outf2, "costDistDf1", suffix1, ".rds")  )




