library(raster)
setwd("~/Experiments/Erim_NGS/Script")
folder<-"../Data/Coastline_Groups/1"
names<-c("australia", "avalonia", "baltica", "gond", "laurentiia",
         "schina", "siberiai")
mask<-raster(sprintf("%s/mask.tif", folder))
plot(mask)
p<-data.frame(rasterToPoints(mask))
p[!is.na(p$mask), "mask"]<-0
n<-names[1]
for (n in names){
  r<-raster(sprintf("%s/%s.asc", folder, n))
  v<-extract(r, p[, c("x", "y")])
  p[which(!is.na(v)), "mask"]<-v[!is.na(v)]
}

values(mask)[!is.na(values(mask))]<-p$mask
plot(mask)
writeRaster(mask, "../Data/Coastline_Groups/1/Coastline_Groups.tif")
