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


dim(ShPathGenDistL2[[i]][[5]])
dim(ShPathGenDistL2[[i]][[6]])
dim(ShPathGenDistL2[[i]][[7]])
dim(ShPathGenDistL2[[i]][[8]])

head(ShPathGenDistL2[[i]][[5]], 6)

par(mfrow=c(1,3))
plot((ShPathGenDistL2[[i]][[5]][,7]) ~ (ShPathGenDistL2[[i]][[6]][,7]))
plot((ShPathGenDistL2[[i]][[5]][,7]) ~ (ShPathGenDistL2[[i]][[7]][,7]))
plot((ShPathGenDistL2[[i]][[5]][,7]) ~ (ShPathGenDistL2[[i]][[8]][,7]))


# snapping turtle
SN2_distance_df0 <- as.data.frame(cbind((ShPathGenDistL2[[i]][[1]][,8]), (ShPathGenDistL2[[i]][[1]][,7]) , (ShPathGenDistL2[[i]][[2]][,7]),
(ShPathGenDistL2[[i]][[3]][,7]),
 (ShPathGenDistL2[[i]][[4]][,7])))


SN2_distance_dfA <- SN2_distance_df 

SN2_distance_df <- as.data.frame(cbind((ShPathGenDistL2[[i]][[5]][,8]), (ShPathGenDistL2[[i]][[5]][,7]) , (ShPathGenDistL2[[i]][[6]][,7]),
(ShPathGenDistL2[[i]][[7]][,7]),
 (ShPathGenDistL2[[i]][[8]][,7])))
 
head(SN2_distance_df ) 
summary(SN2_distance_df)

library(ggplot2)
install.packages("GGally")
library(GGally)

dev.new()
ggpairs(SN2_distance_df, columns = 1:5,
        lower = list(continuous = "smooth"),
        diag = list(continuous = "density"), axisLabels = "show",  title = "Version 7 shrt dist")

dev.new()        
pairs(SN2_distance_df)

dev.new()
ggpairs(SN2_distance_df0, columns = 1:5,
        lower = list(continuous = "smooth"),
        diag = list(continuous = "density"), axisLabels = "show" )

# using means

SN2_distance_df2 <- as.data.frame(cbind((ShPathGenDistL2[[i]][[5]][,8]), (ShPathGenDistL2[[i]][[5]][,11]) , (ShPathGenDistL2[[i]][[6]][,11]),
(ShPathGenDistL2[[i]][[7]][,11]),
 (ShPathGenDistL2[[i]][[8]][,11])))
dev.new() 
 ggpairs(SN2_distance_df2, columns = 1:5,
        lower = list(continuous = "smooth"),
        diag = list(continuous = "density"), axisLabels = "show")


# using medians
SN2_distance_df3 <- as.data.frame(cbind((ShPathGenDistL2[[i]][[5]][,8]), (ShPathGenDistL2[[i]][[5]][,12]) , (ShPathGenDistL2[[i]][[6]][,12]),
(ShPathGenDistL2[[i]][[7]][,12]),
 (ShPathGenDistL2[[i]][[8]][,12])))
 
 summary((ShPathGenDistL2[[i]][[5]][,12]))
 summary((ShPathGenDistL2[[i]][[5]][,11]))
  
 summary((ShPathGenDistL2[[i]][[8]][,12]))
 summary((ShPathGenDistL2[[i]][[8]][,11]))
 
 dev.new();
 dev.new() 
 ggpairs(SN2_distance_df3, columns = 1:5,
        lower = list(continuous = "smooth"),
        diag = list(continuous = "density"), axisLabels = "show",  title = "Median")
        
        
cor.test(x=(ShPathGenDistL2[[i]][[5]][,12]), y=(ShPathGenDistL2[[i]][[6]][,12]), method = 'spearman')     

cor.test(x=(ShPathGenDistL2[[i]][[6]][,12]), y=(ShPathGenDistL2[[i]][[7]][,12]), method = 'spearman')   




rWetNew1 <- ifel(rWet > 0.01, rWet^2, rWet)

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



   
