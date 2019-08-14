library("raster")
library(sp) 
library(ncdf4)
library(maptools)


setwd("~/Experiments/Erim_NGS/Script")

for (sss in c("450Ma_Blakey.CSO.ocean.3XR9", "450Ma_Blakey.CSO.ocean.8XR5", 
              "450Ma_Blakey.HSO.ocean.3XR9", "450Ma_Blakey.HSO.ocean.8XR5")){
  woa13<-nc_open(sprintf("GCMS/%s.nc", sss))
  summary(woa13)
  summary(woa13$dim)
  nbounds<-woa13$dim$nbounds$vals
  lat<-woa13$dim$lat$vals
  lon<-woa13$dim$lon$vals
  lev<-woa13$dim$lev$vals
  time<-woa13$dim$time$vals
  
  
  summary(woa13$var)
  
  varlist<-c("TEMP")
  lon_lat<-expand.grid(lon, lat, lev, time) 
  i_var=1
  for (i_var in c(1:length(varlist))){
    v_var<-varlist[i_var]
    v_woa13<-ncvar_get(woa13, varid=v_var)
    i_lv=1
    i_t=1
    d_all<-data.frame(lon=as.vector(lon_lat$Var1), 
                      lat=as.vector(lon_lat$Var2), 
                      lev=as.vector(lon_lat$Var3), 
                      time=as.vector(lon_lat$Var4))
    
    
    vector_woa13<-as.vector(v_woa13)
    d_all$d<-vector_woa13
    colnames(d_all)[length(colnames(d_all))]<-v_var
    
    for (lv in c(-10)){
      for (i_t in c(1:length(time))){
        
        t<-time[i_t]
        print(sprintf("../Data/GCMS/Ord/%s/%s.t_%d.lev.%f.tif", sss, v_var, t, lv))
        d<-d_all[which((d_all$time==t) & (d_all$lev==lv)),]
        points<-SpatialPointsDataFrame(coords=data.frame(lon=d$lon, d$lat), 		
                                       data=data.frame(d))
        range(lat)
        length(lat)
        range(lon)
        length(lon)
        r <- raster(ncols=length(lon), nrows= length(lat), xmn=0, xmx=360, ymn=-90, ymx=90)
        r <- rasterize(points, r, v_var, fun=mean)
        r  <- rotate(r)
        #plot(r)
        projection(r) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
        writeRaster(r, filename=sprintf("../Data/GCMS/Ord/%s/%s.t_%d.lev.%f.tif", sss, v_var, t, lv), format="GTiff", overwrite=TRUE)
        
      }
    }
    #vector_woa13<-as.vector(v_woa13)
    #d$d<-vector_woa13
    #colnames(d)[length(colnames(d))]<-v_var
  }
}



##average them

layer<-"3XR9"
s<-"HSO"
for (layer in c("3XR9", "8XR5")){
  df<-data.frame()
  for (s in c("HSO", "CSO")){
    
    files<-list.files(sprintf("../Data/GCMS/Ord/450Ma_Blakey.%s.ocean.%s/", s, layer))
    f<-files[1]
    for (f in files){
      print(f)
      r<-raster(sprintf("../Data/GCMS/Ord/450Ma_Blakey.%s.ocean.%s/%s", s, layer, f))
      p<-data.frame(rasterToPoints(r))
      colnames(p)[3]<-"v"
      p$s<-s
      p$layer<-layer
      p$f<-f
      if (nrow(df)==0){
        df<-p
      }else{
        df<-rbind(df, p)
      }
    }
  }
  df_mean<-aggregate(df$v, list(df$x, df$y), mean)
  df_mean<-df_mean[order(-df_mean$Group.2, df_mean$Group.1),]
  values(r)[which(!is.na(values(r)))]<-df_mean$x
  plot(r)
  writeRaster(r, sprintf("../Data/GCMS/Ord/mean/%s_mean.tif", layer), overwrite=T)
  
  r_rough<-r
  
  res(r_rough)<-c(3.75, 2.46575342465753)
  r_rough<-resample(r, r_rough, method="ngb")
  
  plot(r_rough)
  writeRaster(r_rough, filename=sprintf("../Data/GCMS/Ord/mean/%s_mean_rough.tif", layer), 
              format="GTiff", overwrite=TRUE)
  
  r_fine<-r
  
  res(r_fine)<-c(1, 1)
  r_fine<-resample(r, r_fine, method="ngb")
  
  plot(r_fine)
  writeRaster(r_fine, filename=sprintf("../Data/GCMS/Ord/mean/%s_mean_fine.tif", layer), 
              format="GTiff", overwrite=TRUE)
}
