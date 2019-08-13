library(raster)
folder<-"/home/huijieqiao/Erin_To_Rerun/20190813"
dir<-list.dirs(folder)
d<-dir[4]
for (d in dir){
  print(d)
  file<-list.files(d, "\\.tif$")
  if (length(file)==0){
    file<-list.files(d, "\\.asc$")
  }else{
    file<-c(file, list.files(d, "\\.asc$"))
  }
  if (length(file)==0){
    next()
  }
  
  f<-file[1]
  for (f in file){
    #print(f)
    if (!(f %in% c("1.asc", "2.asc", "1.tif", "2.tif"))){
      next()
    }
    print(f)
    r<-raster(sprintf("%s/%s", d, f))
    
    #plot(r)
    v<-values(r)
    if (length(v[which(v==0)])!=0){
      v[which(v==0)]<-NA
    }
    v[which(is.na(v))]<-0
    values(r)<-v
    plot(r)
    r_fine<-r
    res(r_fine)<-c(1,1)
    r_fine<-resample(r, r_fine, method="ngb")
    #plot(r_fine)
    writeRaster(r_fine, filename=sprintf("%s/fine_%s", d, gsub(".asc", ".tif", f)), 
                format="GTiff", overwrite=TRUE)
    
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
    
    ppp<-pp
    
    ppp[which(!is.na(ppp))]<-1
    pp[(rowSums(ppp, na.rm = T)==360),]<-NA
    
    values(r_fine)<-pp
    
    plot(r_fine)
    writeRaster(r_fine, filename=sprintf("%s/fine_coastline_%s", d, gsub(".asc", ".tif", f)), 
                format="GTiff", overwrite=TRUE)
  }
}
