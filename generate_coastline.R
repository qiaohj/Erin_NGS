library("raster")
setwd("~/Experiments/Erim_NGS/Script")
library(sp) 
library(ncdf4)
library(maptools)
library(ggplot2)
rm(list=ls())
detect_coastline<-
  function(f, target){
    print(f)
    r_fine<-raster(f)
    
    p<-as.matrix(r_fine)
    x=2
    y=2
    pp<-p
    for (x in c(1:ncol(p))){
      for (y in c(1:nrow(p))){
        if (is.na(p[y, x])){
          next()
        }
        if (p[y, x]!=0){
          next()
        }
        if (x>1){
          p_n<-p[y, x-1]
          if (p_n==2){
            pp[y, x]<-3
            next()
          }
          if (y>1){
            p_n<-p[y-1, x-1]
            if (p_n==2){
              pp[y, x]<-3
              next()
            }
          }
          if (y<nrow(p)){
            p_n<-p[y+1, x-1]
            if (p_n==2){
              pp[y, x]<-3
              next()
            }
          }
          
          
        }
        if (x<ncol(p)){
          p_n<-p[y, x+1]
          if (p_n==2){
            pp[y, x]<-3
            next()
          }
          if (y>1){
            p_n<-p[y-1, x+1]
            if (p_n==2){
              pp[y, x]<-3
              next()
            }
          }
          if (y<nrow(p)){
            p_n<-p[y+1, x+1]
            if (p_n==2){
              pp[y, x]<-3
              next()
            }
          }
        }
        if (y>1){
          p_n<-p[y-1, x]
          if (p_n==2){
            pp[y, x]<-3
            next()
          }
        }
        if (y<nrow(p)){
          p_n<-p[y+1, x]
          if (p_n==2){
            pp[y, x]<-3
            next()
          }
        }
        
      }
    }
    pp[which(pp==2)]<-NA
    pp[which(pp==0)]<-NA
    
    ppp<-pp
    
    
    
    values(r_fine)<-pp
    
    plot(r_fine)
    writeRaster(r_fine, filename=target, 
                format="GTiff", overwrite=TRUE)
  }

filename<-"tdwzw_bathymetry_latlon.nc"
df_res<-data.frame()
for (filename in c("tdlud_bathymetry_latlon.nc",
                   "tdlup_bathymetry_latlon.nc",
                   "tdqpq_bathymetry_latlon.nc",
                   "tdqps_bathymetry_latlon.nc",
                   "tdwzw_bathymetry_latlon.nc",
                   "teckg_bathymetry_latlon.nc",
                   "teckm_bathymetry_latlon.nc",
                   "tecqa_bathymetry_latlon.nc",
                   "Mask_0_200m_bathy_cells_Eocene.nc",
                   "Mask_0_200m_bathy_cells_Oligocene.nc",
                   "postSLdrop_0openocean_1shallowshelf_2land.nc",
                   "preSLdrop_0openocean_1shallowshelf_2land.nc")){
  print(filename)
  cdf_file<-nc_open(sprintf("../Data/Depth/nc/%s", filename))
  if (filename %in% c("tdlud_bathymetry_latlon.nc",
                      "tdlup_bathymetry_latlon.nc",
                      "tdqpq_bathymetry_latlon.nc",
                      "tdqps_bathymetry_latlon.nc",
                      "tdwzw_bathymetry_latlon.nc",
                      "teckg_bathymetry_latlon.nc",
                      "teckm_bathymetry_latlon.nc",
                      "tecqa_bathymetry_latlon.nc")){
    var<-"depthdepth"
    lat<-cdf_file$dim$latitude$vals
    lon<-cdf_file$dim$longitude$vals
  }
  if (filename %in% c("Mask_0_200m_bathy_cells_Eocene.nc",
                      "Mask_0_200m_bathy_cells_Oligocene.nc")){
    if (filename=="Mask_0_200m_bathy_cells_Eocene.nc"){
      var<-"Mask_Eocene"
    }
    if (filename=="Mask_0_200m_bathy_cells_Oligocene.nc"){
      var<-"Mask_Oligocene"
    }
    lat<-cdf_file$dim$lat$vals
    lon<-cdf_file$dim$lon$vals
  }
  
  if (filename %in% c("postSLdrop_0openocean_1shallowshelf_2land.nc",
                      "preSLdrop_0openocean_1shallowshelf_2land.nc")){
    if (filename=="postSLdrop_0openocean_1shallowshelf_2land.nc"){
      var<-"POSTSLDROP"
    }
    if (filename=="preSLdrop_0openocean_1shallowshelf_2land.nc"){
      var<-"PRESLDROP"
    }
    lat<-cdf_file$dim$MYLAT$vals
    lon<-cdf_file$dim$MYLON$vals
  }
  
  
  lon_lat<-expand.grid(lon, lat)
  
  d<-data.frame(lon=as.vector(lon_lat$Var1), lat=as.vector(lon_lat$Var2))
  v<-ncvar_get(cdf_file, varid=var)
  v<-as.vector(v)
  d$v<-v
  points<-SpatialPointsDataFrame(coords=data.frame(d$lon, d$lat), data=data.frame(d))
  #ggplot(d)+geom_point(aes(x=lon, y=lat, color=factor(v)))
  range(lat)
  length(lat)
  range(lon)
  length(lon)
  
  r <- raster(ncols=length(lon), nrows= length(lat), xmn=-180, xmx=180, ymn=-90, ymx=90)
  if (filename %in% c("postSLdrop_0openocean_1shallowshelf_2land.nc",
                      "preSLdrop_0openocean_1shallowshelf_2land.nc")){
    r <- raster(ncols=length(lon), nrows= length(lat), xmn=0, xmx=360, ymn=-90, ymx=90)
  }
  r <- rasterize(points, r, v, fun=mean)
  item<-data.frame(length_lon=length(lon), length_lat=length(lat),
                   res_lon=res(r)[1], res_lat=res(r)[2],
                   min_lon=min(lon), max_lon=max(lon),
                   min_lat=min(lat), max_lat=max(lat),
                   filename=filename)
  if (nrow(df_res)==0){
    df_res<-item
  }else{
    df_res<-rbind(df_res, item)
  }
  #plot(r)
  if (filename %in% c("postSLdrop_0openocean_1shallowshelf_2land.nc",
                      "preSLdrop_0openocean_1shallowshelf_2land.nc")){
    r <- rotate(r)
  }
  
  #
  #plot(r)
  #writeRaster(r, filename="postSLdrop_0openocean_1shallowshelf_2land_rough_res.tif", 
  #            format="GTiff", overwrite=TRUE)
  
  
  
  writeRaster(r, filename=sprintf("../Data/Depth/tif/%s", gsub(".nc", ".tif", filename)), 
              format="GTiff", overwrite=TRUE)
  if (filename %in% c("tdlud_bathymetry_latlon.nc",
                      "tdlup_bathymetry_latlon.nc",
                      "tdqpq_bathymetry_latlon.nc",
                      "tdqps_bathymetry_latlon.nc",
                      "tdwzw_bathymetry_latlon.nc",
                      "teckg_bathymetry_latlon.nc",
                      "teckm_bathymetry_latlon.nc",
                      "tecqa_bathymetry_latlon.nc")){
    threshold<-200
    epeiric_sea<-r
    v<-values(epeiric_sea)
    v[which(v<=threshold)]<-1 #epeiric sea
    v[which(v>threshold)]<-0 #open sea
    v[which(is.na(v))]<-2 # continent
    values(epeiric_sea)<-v
  }
  if (filename %in% c("Mask_0_200m_bathy_cells_Eocene.nc",
                      "Mask_0_200m_bathy_cells_Oligocene.nc",
                      "postSLdrop_0openocean_1shallowshelf_2land.nc",
                      "preSLdrop_0openocean_1shallowshelf_2land.nc")){
    epeiric_sea<-r
  }
  
  writeRaster(epeiric_sea, filename=sprintf("../Data/Epeiric_Sea/%s", gsub(".nc", "_epeiric_sea.tif", filename)), 
              format="GTiff", overwrite=TRUE)
  
  r_rough<-epeiric_sea
  res(r_rough)<-c(3.75, 2.46575342465753)
  r_rough<-resample(epeiric_sea, r_rough, method="ngb")
  
  plot(r_rough)
  writeRaster(r_rough, filename=sprintf("../Data/Epeiric_Sea/%s", gsub(".nc", "_epeiric_sea_rough.tif", filename)), 
              format="GTiff", overwrite=TRUE)
  
  
  detect_coastline(sprintf("../Data/Epeiric_Sea/%s", gsub(".nc", "_epeiric_sea_rough.tif", filename)),
                   sprintf("../Data/Coastline/%s", gsub(".nc", "_coastline_rough.tif", filename)))
  
  coastline<-raster(sprintf("../Data/Coastline/%s", gsub(".nc", "_coastline_rough.tif", filename)))
  
  
  r_fine<-coastline
  res(r_fine)<-c(1,1)
  r_fine<-resample(coastline, r_fine, method="ngb")
  
  
  pp<-as.matrix(r_fine)
  pp[which(!is.na(pp))]<-1
  pp[(rowSums(pp, na.rm = T)==360),]<-NA
  values(r_fine)<-pp
  plot(r_fine)
  writeRaster(r_fine, filename=sprintf("../Data/Coastline/%s", gsub(".nc", "_coastline_fine.tif", filename)), 
              format="GTiff", overwrite=TRUE)
  
  r_fine<-epeiric_sea
  res(r_fine)<-c(1,1)
  r_fine<-resample(epeiric_sea, r_fine, method="ngb")
  writeRaster(r_fine, filename=sprintf("../Data/Epeiric_Sea/%s", gsub(".nc", "_epeiric_sea_fine.tif", filename)), 
              format="GTiff", overwrite=TRUE)
  detect_coastline(sprintf("../Data/Epeiric_Sea/%s", gsub(".nc", "_epeiric_sea_fine.tif", filename)),
                   sprintf("../Data/Coastline/%s", gsub(".nc", "_coastline_fine_2.tif", filename)))
  
  r_fine<-raster(sprintf("../Data/Coastline/%s", gsub(".nc", "_coastline_fine_2.tif", filename)))
  pp<-as.matrix(r_fine)
  pp[which(!is.na(pp))]<-1
  pp[(rowSums(pp, na.rm = T)==360),]<-NA
  values(r_fine)<-pp
  plot(r_fine)
  writeRaster(r_fine, filename=sprintf("../Data/Coastline/%s", gsub(".nc", "_coastline_fine_2.tif", filename)), 
              format="GTiff", overwrite=TRUE)
  
}
write.table(df_res, "../Tables/raw_res.csv",  row.names=F, sep=",")
