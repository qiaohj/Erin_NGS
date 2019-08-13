library("raster")
setwd("~/Downloads")
library(sp) 
library(ncdf4)
library(maptools)
library(ggplot2)

#cdf_file<-nc_open("postSLdrop_0openocean_1shallowshelf_2land.nc")
cdf_file<-nc_open("preSLdrop_0openocean_1shallowshelf_2land.nc")
summary(cdf_file)
lat<-cdf_file$dim$MYLAT$vals
lon<-cdf_file$dim$MYLON$vals
lon_lat<-expand.grid(lon, lat)

d<-data.frame(lon=as.vector(lon_lat$Var1), lat=as.vector(lon_lat$Var2))
v<-ncvar_get(cdf_file, varid="PRESLDROP")
#v<-ncvar_get(cdf_file, varid="POSTSLDROP")
v<-as.vector(v)
d$v<-v
points<-SpatialPointsDataFrame(coords=data.frame(d$lon, d$lat), data=data.frame(d))
ggplot(d)+geom_point(aes(x=lon, y=lat, color=factor(v)))
range(lat)
length(lat)
range(lon)
length(lon)


r <- raster(ncols=128, nrows=128, xmn=0, xmx=360, ymn=-90, ymx=90)
r <- rasterize(points, r, v)
plot(r)
r <- rotate(r)
plot(r)
#writeRaster(r, filename="postSLdrop_0openocean_1shallowshelf_2land_low_res.tif", 
#            format="GTiff", overwrite=TRUE)
writeRaster(r, filename="preSLdrop_0openocean_1shallowshelf_2land_low_res.tif", 
            format="GTiff", overwrite=TRUE)
r_fine<-r
res(r_fine)<-c(1,1)
r_fine<-resample(r, r_fine, method="ngb")
plot(r_fine)
writeRaster(r_fine, filename="preSLdrop_0openocean_1shallowshelf_2land_high_res.tif", 
            format="GTiff", overwrite=TRUE)
