library("raster")
library(sp) 
library(ncdf4)
library(maptools)


setwd("~/Experiments/Erim_NGS/Script")
woa13_str<-"../Data/GCMS/Alex_Eoc/tdludo.pfclann_formatted_sst.nc"
for (woa13_str in c("../Data/GCMS/Alex_Eoc/tdludo.pfclann_formatted_sst.nc",
                    "../Data/GCMS/Alex_Eoc/tdlupo.pfclann_formatted_sst.nc",
                    "../Data/GCMS/Alex_Plio/tdqpqo.pfclann_formatted_sst.nc",
                    "../Data/GCMS/Alex_Plio/tdqpso.pfclann_formatted_sst.nc",
                    "../Data/GCMS/Paul_Plio/tdwzwo.pfclann_formatted_sst.nc",
                    "../Data/GCMS/Paul_Plio/teckgo.pfclann_formatted_sst.nc",
                    "../Data/GCMS/Paul_Eoc/teckmo.pfclann_formatted_sst.nc",
                    "../Data/GCMS/Paul_Eoc/tecqao.pfclann_formatted_sst.nc")){
  print(woa13_str)
  woa13<-nc_open(woa13_str)
  lat<-woa13$dim$lat$vals
  lon<-woa13$dim$lon$vals
  depth_1<-woa13$dim$depth_1$vals
  time<-woa13$dim$t$vals
  lon_lat<-expand.grid(lon, lat) 
  v_woa13<-ncvar_get(woa13, varid="temp_mm_dpth")
  d_all<-data.frame(lon=as.vector(lon_lat$Var1), 
                    lat=as.vector(lon_lat$Var2))
  
  vector_woa13<-as.vector(v_woa13)
  d_all$d<-vector_woa13
  colnames(d_all)[length(colnames(d_all))]<-"temp_mm_dpth"
  
  d<-d_all
  points<-SpatialPointsDataFrame(coords=data.frame(lon=d$lon, d$lat), 		
                                 data=data.frame(d))
  range(lat)
  length(lat)
  range(lon)
  length(lon)
  r <- raster(ncols=length(lon), nrows=length(lat), xmn=-180, xmx=180, ymn=-90, ymx=90)
  r <- rasterize(points, r, "temp_mm_dpth", fun=mean)
  #r  <- rotate(r)
  #plot(r)
  projection(r) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  writeRaster(r, filename=gsub(".nc", ".tif", woa13_str), format="GTiff", overwrite=TRUE)
  if (F){
    if (woa13_str=="../Data/GCMS/Paul_Plio/tdwzwo.pfclann_formatted_sst.nc"){
      r_rough<-r
      res(r_rough)<-c(3.75, 2.46575342465753)
      r_rough<-resample(r, r_rough, method="ngb")
      
      plot(r_rough)
      writeRaster(r_rough, filename=gsub(".nc", "_rough.tif", woa13_str), 
                  format="GTiff", overwrite=TRUE)
      r<-r_rough
    }
  }
  r_fine<-r
  
  res(r_fine)<-c(1, 1)
  r_fine<-resample(r, r_fine, method="ngb")
  
  plot(r_fine)
  writeRaster(r_fine, filename=gsub(".nc", "_fine.tif", woa13_str), 
              format="GTiff", overwrite=TRUE)
  
}

