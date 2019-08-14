library(raster)
df<-read.table("../Tables/layer_mapping.csv", head=T, sep=",", stringsAsFactors = F)
setwd("~/Experiments/Erim_NGS/Script")
i=1

for (i in c(1:nrow(df))){
  item<-df[i,]
  coastline1<-raster(item$coastline1)
  
  gradient1<-raster(item$gradient1)
  
  p<-data.frame(rasterToPoints(coastline1))
  colnames(p)[3]<-"coastline"
  unique(p$coastline)
  p$gradient<-extract(gradient1, p[, c("x", "y")])
  values(coastline1)[which(!is.na(values(coastline1)))]<-round(p$gradient * 1000)
  plot(coastline1)
  dir.create(sprintf("../Data/Env/%s", item$S), showWarnings = F)
  dir.create(sprintf("../Data/Env/%s/%s", item$S, item$P), showWarnings = F)
  dir.create(sprintf("../Data/Env/%s/%s/env", item$S, item$P), showWarnings = F)
  writeRaster(coastline1, sprintf("../Data/Env/%s/%s/env/1200.tif", item$S, item$P), overwrite=TRUE)
  
  coastline2<-raster(item$coastline2)
  
  gradient2<-raster(item$gradient2)
  
  p<-data.frame(rasterToPoints(coastline2))
  colnames(p)[3]<-"coastline"
  unique(p$coastline)
  p$gradient<-extract(gradient2, p[, c("x", "y")])
  values(coastline2)[which(!is.na(values(coastline2)))]<-round(p$gradient * 1000)
  plot(coastline2)
  dir.create(sprintf("../Data/Env/%s", item$S), showWarnings = F)
  dir.create(sprintf("../Data/Env/%s/%s", item$S, item$P), showWarnings = F)
  dir.create(sprintf("../Data/Env/%s/%s/env", item$S, item$P), showWarnings = F)
  writeRaster(coastline2, sprintf("../Data/Env/%s/%s/env/1199.tif", item$S, item$P), overwrite=TRUE)
  
}
