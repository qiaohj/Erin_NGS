library(raster)
df<-read.table("../Tables/layer_mapping.csv", head=T, sep=",", stringsAsFactors = F)
i=1
result<-data.frame()
for (i in c(1:nrow(df))){
  item<-df[i,]
  coastline1<-raster(item$coastline1)
  
  gradient1<-raster(item$gradient1)
  
  p<-data.frame(rasterToPoints(coastline1))
  colnames(p)[3]<-"coastline"
  unique(p$coastline)
  p$gradient<-extract(gradient1, p[, c("x", "y")])
  
  p$gradient_v<-F
  p[which(!is.na(p$gradient)),]$gradient_v<-T
  vvv<-data.frame(table(p$gradient_v))
  vvv$Var1<-as.logical(vvv$Var1)
  if (nrow(vvv[which(vvv$Var1==F),])==0){
    vvv<-rbind(vvv, data.frame(Var1=F, Freq=0))
  }
  item$coastline1_in_gradient<-vvv[which(vvv$Var1==T), "Freq"]
  item$coastline1_out_gradient<-vvv[which(vvv$Var1==F), "Freq"]
  
  coastline2<-raster(item$coastline2)
  
  gradient2<-raster(item$gradient2)
  
  p<-data.frame(rasterToPoints(coastline2))
  colnames(p)[3]<-"coastline"
  unique(p$coastline)
  p$gradient<-extract(gradient2, p[, c("x", "y")])
  
  p$gradient_v<-F
  p[which(!is.na(p$gradient)),]$gradient_v<-T
  vvv<-data.frame(table(p$gradient_v))
  vvv$Var1<-as.logical(vvv$Var1)
  if (nrow(vvv[which(vvv$Var1==F),])==0){
    vvv<-rbind(vvv, data.frame(Var1=F, Freq=0))
  }
  item$coastline2_in_gradient<-vvv[which(vvv$Var1==T), "Freq"]
  item$coastline2_out_gradient<-vvv[which(vvv$Var1==F), "Freq"]
  if (nrow(result)==0){
    result<-item
  }else{
    result<-rbind(result, item)
  }
}
write.table(result, "../Tables/layer_mapping_with_N.csv", row.names=F, sep=",")




