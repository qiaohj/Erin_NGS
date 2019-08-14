
library("raster")
library(sp) 
library(ncdf4)
library(maptools)


setwd("~/Experiments/Erim_NGS/Script")

df<-read.table("../Tables/raw_res.csv", head=T, sep=",", stringsAsFactors = F)

i=1
for (i in c(1:nrow(df))){
  if (df[i, "Type"]!="GCM"){
    next()
  }
  woa13<-nc_open(sprintf(df[i, "filename"]))
  lat<-woa13$dim$lat$vals
  lon<-woa13$dim$lon$vals
  print(range(lat))
  print(range(lon))
  print(length(lat))
  print(length(lon))
  df[i, "length_lon"]<-length(lon)
  df[i, "length_lat"]<-length(lat)
  df[i, "min_lon"]<-min(lon)
  df[i, "max_lon"]<-max(lon)
  df[i, "min_lat"]<-min(lat)
  df[i, "max_lat"]<-max(lat)
}
write.table(df, "../Tables/raw_res.csv", row.names=F, sep=",")
