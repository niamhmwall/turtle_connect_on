df1 <- (ShPathGenDistL2[[i]][[4]])

cor(df1[,7], df1[,8], na.rm=T)

dev.new(); plot(df1[,7], df1[,8])

cor(df1[,7], df1[,8], method = "pearson", use = "complete.obs")

 df2 <- df1[which(df1$distance>0),]
 df2[1:20,]
 dim(df1)
 
 cor(df2[,7], df2[,8], method = "pearson", use = "complete.obs")
 dev.new(); plot(df2[,7], df2[,8], main="With dups")
 
 df3 <- df2[, 7:8]
 
 dim(df3)
 dim(df3[!duplicated(df3),])
 head(df3, 20) 
 
df4 <- (df3[!duplicated(df3),])

 cor(df4[,1], df4[,2], method = "pearson", use = "complete.obs")
 
  dev.new(); plot(df4[,1], df4[,2], main="With NO dups")
  summary(df4)
  
  
  df1800 <- (ShPathGenDistL2[[i]][[1]])
  dim(df1800)
  head(df1800, 20)
 df1800 <- df1800[which(df1800$distance>0),7:8]
df1800 <- (df1800[!duplicated(df1800),])
 cor(df1800[,1], df1800[,2], method = "pearson", use = "complete.obs")
 
  dev.new(); plot(df1800[,1], df1800[,2], main="With NO dups")
  summary(df1800)
  dev.new(); boxplot(df1800)
  
  
  df1 <- (ShPathGenDistL2[[i]][[4]])
 
 
dim(SN2_distance_df)
summary(SN2_distance_df)  
dfSN1 <- (SN2_distance_df[which(SN2_distance_df$V1>0),])
head(dfSN1)
dev.new(); boxplot(dfSN1)

SN2_distance_df0 <- as.data.frame(cbind((ShPathGenDistL2[[i]][[5]][,8]), (ShPathGenDistL2[[i]][[1]][,7]) , (ShPathGenDistL2[[i]][[2]][,7]),
(ShPathGenDistL2[[i]][[3]][,7]),
 (ShPathGenDistL2[[i]][[4]][,7])))
 
dfSN0 <- (SN2_distance_df0[which(SN2_distance_df0$V1>0),])
summary(dfSN0)
head(dfSN0)
dev.new(); boxplot(dfSN0)
 



