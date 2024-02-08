

#------------------------------------------- 
# Exploratory plots                                   

# Shortest path plots with conductance layer
dev.new()
raster::plot(raster::raster(tr.cost.AllL[[1]]), xlab="x coordinate (m)", ylab="y coordinate (m)", legend.lab="Conductance", main="Wetland 1800")
lines(shPathLAll[[1]][[1]][[1]][[4]], col="red", lwd=2 )
#lines(shPathLAll[[1]][[1]][[1]][[4]], col="black", lwd=2 )  # 5km
lines(shPathLAll[[1]][[1]][[1]][[2]], col="black", lwd=2 )
lines(shPathLAll[[1]][[1]][[1]][[3]], col="blue", lwd=2 ) 
lines(shPathLAll[[1]][[1]][[1]][[5]], col="magenta", lwd=2 )  
lines(shPathLAll[[1]][[1]][[1]][[6]], col="green", lwd=2 )  

# 2002
dev.new()
raster::plot(raster::raster(tr.cost.AllL[[4]]), xlab="x coordinate (m)", ylab="y coordinate (m)", legend.lab="Conductance", main="Wetland 2002")
lines(shPathLAll[[1]][[4]][[1]][[4]], col="red", lwd=2 )
#lines(shPathLAll[[1]][[1]][[1]][[4]], col="black", lwd=2 )  # 5km
lines(shPathLAll[[1]][[4]][[1]][[2]], col="black", lwd=2 )
lines(shPathLAll[[1]][[4]][[1]][[3]], col="blue", lwd=2 ) 
lines(shPathLAll[[1]][[4]][[1]][[5]], col="magenta", lwd=2 )  
lines(shPathLAll[[1]][[4]][[1]][[6]], col="green", lwd=2 ) 

# 
lines(shPathLAll[[3]][[1]][[5]][[6]], col="red", lwd=2 )  # LH2 to GB2 in 1800 
lines(shPathLAll[[3]][[2]][[5]][[6]], col="blue", lwd=2 )  # LH2 to GB2 in 1967

lines(shPathLAll[[3]][[1]][[5]][[13]], col="magenta", lwd=2 )  # LH2 to HU in 1800
lines(shPathLAll[[3]][[2]][[5]][[13]], col="turquoise", lwd=2 ) # LH2 to HU in 1967
plot(st_geometry(pts_pjL[[3]][5,]), pch=3, cex=1.5, add=TRUE)
plot(st_geometry(pts_pjL[[3]][13,]), pch=1, cex=1.5, add=TRUE)

#text(pts_pjL[[1]][,"Site"]) 



#--------------------------------------------------------------
# Correlation plots

library(ggplot2)
#install.packages("GGally")
library(GGally)


# explore objects
class(ShPathGenDistL2[[i]][[1]])

summary(ShPathGenDistL2[[i]][[1]])
summary(ShPathGenDistL2[[i]][[2]])
summary(ShPathGenDistL2[[i]][[3]])
summary(ShPathGenDistL2[[i]][[4]])

head(ShPathGenDistL2[[i]][[1]], 20)


summary(ShPathGenDistL2[[i]][[5]])
summary(ShPathGenDistL2[[i]][[6]])
summary(ShPathGenDistL2[[i]][[7]])
summary(ShPathGenDistL2[[i]][[8]])

head(ShPathGenDistL2[[i]][[5]], 6)

par(mfrow=c(1,3))
plot((ShPathGenDistL2[[i]][[5]][,7]) ~ (ShPathGenDistL2[[i]][[6]][,7]))
plot((ShPathGenDistL2[[i]][[5]][,7]) ~ (ShPathGenDistL2[[i]][[7]][,7]))
plot((ShPathGenDistL2[[i]][[5]][,7]) ~ (ShPathGenDistL2[[i]][[8]][,7]))



# Shortest paths
SN_distance_df <- as.data.frame(cbind((ShPathGenDistL2[[i]][[5]][,8]), (ShPathGenDistL2[[i]][[5]][,7]) , (ShPathGenDistL2[[i]][[6]][,7]),
(ShPathGenDistL2[[i]][[7]][,7]),
 (ShPathGenDistL2[[i]][[8]][,7])))
 
head(SN_distance_df) 
summary(SN_distance_df)


dev.new()
ggpairs(SN_distance_df, columns = 1:5,
        lower = list(continuous = "smooth"),
        diag = list(continuous = "density"), axisLabels = "show",  title = "Version 8 with 0.1s shortest path")

#dev.new()        
#pairs(SN_distance_df)


# using means

SN_distance_df2 <- as.data.frame(cbind((ShPathGenDistL2[[i]][[1]][,8]), (ShPathGenDistL2[[i]][[1]][,11]) , (ShPathGenDistL2[[i]][[2]][,11]),
(ShPathGenDistL2[[i]][[3]][,11]),
 (ShPathGenDistL2[[i]][[4]][,11])))
dev.new() 
 ggpairs(SN_distance_df2, columns = 1:5,
        lower = list(continuous = "smooth"),
        diag = list(continuous = "density"), axisLabels = "show", title="Means")


# using medians
SN_distance_df3 <- as.data.frame(cbind((ShPathGenDistL2[[i]][[1]][,8]), (ShPathGenDistL2[[i]][[1]][,12]) , (ShPathGenDistL2[[i]][[2]][,12]),
(ShPathGenDistL2[[i]][[3]][,12]),
 (ShPathGenDistL2[[i]][[4]][,12])))
 
 dev.new() 
 ggpairs(SN_distance_df3, columns = 1:5,
        lower = list(continuous = "smooth"),
        diag = list(continuous = "density"), axisLabels = "show",  title = "Median")
        
        
#cor.test(x=(ShPathGenDistL2[[i]][[5]][,12]), y=(ShPathGenDistL2[[i]][[6]][,12]), method = 'spearman')     
#cor.test(x=(ShPathGenDistL2[[i]][[6]][,12]), y=(ShPathGenDistL2[[i]][[7]][,12]), method = 'spearman')   


#----------------------------------------------
# using shortest path vs. means

SN2_distance_df2 <- as.data.frame(cbind((ShPathGenDistL2[[i]][[5]][,7]), (ShPathGenDistL2[[i]][[5]][,11]) , (ShPathGenDistL2[[i]][[6]][,11]),
(ShPathGenDistL2[[i]][[7]][,11]),
 (ShPathGenDistL2[[i]][[8]][,11])))
dev.new() 
 ggpairs(SN2_distance_df2, columns = 1:5,
        lower = list(continuous = "smooth"),
        diag = list(continuous = "density"), axisLabels = "show", title = "Mean" )
        
        
 # using shortest path vs. means

SN2_distance_df2 <- as.data.frame(cbind((ShPathGenDistL2[[i]][[5]][,7]), (ShPathGenDistL2[[i]][[5]][,12]) , (ShPathGenDistL2[[i]][[6]][,12]),
(ShPathGenDistL2[[i]][[7]][,12]),
 (ShPathGenDistL2[[i]][[8]][,12])))
dev.new() 
 ggpairs(SN2_distance_df2, columns = 1:5,
        lower = list(continuous = "smooth"),
        diag = list(continuous = "density"), axisLabels = "show", title = "Median" )


#====================================================
# Plot using cost & cummulative Distances

#summary(ShPathGenDistL20[[i]][[1]])
#summary(ShPathGenDistL20[[i]][[2]])
#summary(ShPathGenDistL20[[i]][[3]])
#summary(ShPathGenDistL20[[i]][[4]])
#
#head(ShPathGenDistL2[[i]][[1]], 20)
#
#
#summary(ShPathGenDistL20[[i]][[5]])
#summary(ShPathGenDistL20[[i]][[6]])
#summary(ShPathGenDistL20[[i]][[7]])
#summary(ShPathGenDistL20[[i]][[8]])


#hist(ShPathGenDistL20[[i]][[1]][,"cost_dis"], breaks=10, main="Cost distance")
#dev.new(); hist(ShPathGenDistL20[[i]][[1]][,"distance"], breaks=10, main="Sh Path distance")

 SN2_distance_df <- as.data.frame(cbind((ShPathGenDistL20[[i]][[1]][,8]), (ShPathGenDistL20[[i]][[1]][,15]) , (ShPathGenDistL20[[i]][[2]][,15]),
(ShPathGenDistL20[[i]][[3]][,15]),
 (ShPathGenDistL20[[i]][[4]][,15])))
 head( SN2_distance_df)
 summary(SN2_distance_df)

 dev.new()
 ggpairs(SN2_distance_df, columns = 2:4,
        lower = list(continuous = "smooth"),
        diag = list(continuous = "density"), axisLabels = "show",  title = "Version 8 with 0.1s Cost dist")


 SN2_distance_df <- as.data.frame(cbind((ShPathGenDistL20[[i]][[1]][,8]), (ShPathGenDistL20[[i]][[1]][,16]) , (ShPathGenDistL20[[i]][[2]][,16]),
(ShPathGenDistL20[[i]][[3]][,16]),
 (ShPathGenDistL20[[i]][[4]][,16])))
 head(SN2_distance_df)
 summary(SN2_distance_df)

 dev.new()
 ggpairs(SN2_distance_df, columns = 2:4,
        lower = list(continuous = "smooth"),
        diag = list(continuous = "density"), axisLabels = "show",  title = "Version 8 with 0.1s Cummu dist")


# removing duplicates
dfT1 <- cbind.data.frame("path_id"=(ShPathGenDistL20[[i]][[1]][,9]), "wet1800"=(ShPathGenDistL20[[i]][[1]][,16]) , "wet1967"=(ShPathGenDistL20[[i]][[2]][,16]))
head(dfT1)
dim(dfT1)
dfT1 <- dfT1[which(dfT1$wet1800>0),]
dfT1 <- (dfT1[!duplicated(dfT1),])
dfT1 <- dfT1[1:20,]
dfT1 <- dfT1[21:40,]
dfT1 <- dfT1[41:60,]
dev.new() #; plot(dfT1[,2], dfT1[,3], main="Cummulative distance", xlab="wet1800", ylab="wet1967")
plot(log1p(dfT1[,2]), log1p(dfT1[,3]), main="Cummulative distance", xlab="wet1800", ylab="wet1967")
text(log1p(dfT1[,2]), log1p(dfT1[,3]), dfT1[,1], cex=0.7, pos=1)
#lines(log1p(dfT1[,2]),log1p(dfT1[,2]))
abline(0,1)

cor(dfT1[,1], dfT1[,2], method = "pearson", use = "complete.obs")
#0.144145  
cor.test(dfT1[,1], dfT1[,2], method = "pearson", use = "complete.obs")
#p-value = 0.208  
  

# shortest path  
dfT2 <- cbind.data.frame("path_id"=(ShPathGenDistL20[[i]][[1]][,9]), "wet1800"=(ShPathGenDistL20[[i]][[1]][,7]) , "wet1967"=(ShPathGenDistL20[[i]][[2]][,7]))

head(dfT2)
dim(dfT2)
dfT2 <- dfT2[which(dfT2$wet1800>0),]
#dfT1 <- (dfT1[!duplicated(dfT1),])
dfT1 <- dfT1[1:20,]
dfT1 <- dfT1[21:40,]
dfT2 <- dfT2[41:60,]
dev.new(); 

#plot(dfT2[,2], dfT2[,3], main="Shortest Path 1800 vs. 1967")
#text(dfT2[,2], dfT2[,3], dfT2[,1] )

plot(log1p(dfT2[,2]), log1p(dfT2[,3]),  main="Shotest path", xlab="wet1800", ylab="wet1967")
text(log1p(dfT2[,2]), log1p(dfT2[,3]), dfT2[,1],  cex=0.7, pos=1)
#lines(log1p(dfT2[,2]),log1p(dfT2[,2]))
abline(0,1)

# par=par(mfrow=c(1,2))

# Cost distance
dfT3 <- cbind.data.frame("path_id"=(ShPathGenDistL20[[i]][[1]][,9]), "wet1800"=(ShPathGenDistL20[[i]][[1]][,15]) , "wet1967"=(ShPathGenDistL20[[i]][[2]][,15]))
head(dfT3)
dim(dfT3)
dfT3 <- dfT3[which(dfT3$wet1800>0),]
dfT3 <- dfT3[41:60,]
dev.new() #; plot(dfT1[,2], dfT1[,3], main="Cummulative distance", xlab="wet1800", ylab="wet1967")
plot(log1p(dfT3[,2]), log1p(dfT3[,3]), main="Cost distance", xlab="wet1800", ylab="wet1967")
text(log1p(dfT3[,2]), log1p(dfT3[,3]), dfT3[,1], cex=0.7, pos=1)
#lines(log1p(dfT1[,2]),log1p(dfT1[,2]))
abline(0,1)


# Cumm cost change versus FST

dfT4 <- cbind.data.frame("path_id"=(ShPathGenDistL20[[i]][[1]][,9]), "wet1800"=(ShPathGenDistL20[[i]][[1]][,16]) , "wet1967"=(ShPathGenDistL20[[i]][[2]][,16]), "fst"=ShPathGenDistL20[[i]][[1]][,13])
head(dfT4)

dim(dfT4)
dfT4 <- dfT4[which(dfT4$wet1800>0),]
dfT4$delta1 <- (dfT4$wet1967-dfT4$wet1800)/dfT4$wet1800*100
summary(dfT4)
hist(dfT4$delta1)

dfT4 <- dfT4[41:60,]
dev.new()
plot((dfT4[,5]), (dfT4[,4]), main="Cummulative distance vs. FST", xlab="Change in cumm dist", ylab="fst")
text((dfT4[,5]), (dfT4[,4]), dfT4[,1], cex=0.7, pos=1)
abline(0,1)


# Cost change versus FST

dfT4 <- cbind.data.frame("path_id"=(ShPathGenDistL20[[i]][[1]][,9]), "wet1800"=(ShPathGenDistL20[[i]][[1]][,15]) , "wet1967"=(ShPathGenDistL20[[i]][[2]][,15]), "fst"=ShPathGenDistL20[[i]][[1]][,13])
head(dfT4)

dim(dfT4)
dfT4 <- dfT4[which(dfT4$wet1800>0),]
dfT4$delta1 <- (dfT4$wet1967-dfT4$wet1800)/dfT4$wet1800*100
summary(dfT4)
hist(dfT4$delta1)
#dfT4 <- dfT4[which(dfT4$delta1>0),]

dfT4 <- dfT4[21:40,]
dev.new()
plot((dfT4[,5]), (dfT4[,4]), main="Cost distance vs. FST", xlab="Change in cost dist", ylab="fst")
text((dfT4[,5]), (dfT4[,4]), dfT4[,1], cex=0.7, pos=1)
abline(0,1)


lm1 <- lm((dfT4[,4]) ~ dfT4[,5], data = dfT4) #Create the linear regression
summary(lm1) #Review the results  

# Plots
dfT1 <- cbind.data.frame("path_id"=(ShPathGenDistL20[[i]][[1]][,9]), "wet1800"=(ShPathGenDistL20[[i]][[1]][,16]) , "wet1967"=(ShPathGenDistL20[[i]][[4]][,16]))
head(dfT1)
dim(dfT1)
dfT1 <- dfT1[which(dfT1$wet1800>0),]
dfT1 <- dfT1[41:60,]
dev.new()
plot(log1p(dfT1[,2]), log1p(dfT1[,3]), main="Cummulative distance", xlab="wet1800", ylab="wet1967")
text(log1p(dfT1[,2]), log1p(dfT1[,3]), dfT1[,1], cex=0.7, pos=1)
abline(0,1)
