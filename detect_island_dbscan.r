library("dbscan")
library("fpc")
library("raster")
library("factoextra")
library(RColorBrewer)
setwd("~/Experiments/Erim_NGS/Script")

result<-data.frame()
threshold<-104
for (s in c("Eoc", "Ord", "Plio")){
  r<-raster(sprintf("../Data/Mask/%s.tif", s))
  plot(r)
  p<-data.frame(rasterToPoints(r))
  head(p)
  p_dbscan<-dbscan::dbscan(p[, c("x", "y")], 1.1, minPts = 2)
  p$group<-predict(p_dbscan, data=p[, c("x", "y")])
  
  values(r)[which(!is.na(values(r)))]<-p$group
  plot(r)
  writeRaster(r, sprintf("../Data/island/%s.tif", s), overwrite=T)
  
  item<-data.frame(table(p$group))
  colnames(item)[1]<-"island_no"
  item$S<-s
  if (nrow(result)==0){
    result<-item
  }else{
    result<-rbind(result, item)
  }
  p$island<-1
  p[which(p$group %in% item[which(item$Freq>threshold), "island_no"]), "island"]<-0
  
  values(r)[which(!is.na(values(r)))]<-p$island
  plot(r)
  writeRaster(r, sprintf("../Data/island/%s_bin.tif", s), overwrite=T)
}
write.table(result, "../Tables/N_Pixels_Per_Island.csv", row.names=F, sep=",")
