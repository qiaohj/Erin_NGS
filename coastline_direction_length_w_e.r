library("raster")
setwd("~/Erin_To_Rerun/Script")
library("raster")



targets<-c("../20190607", "../20190612")
folders<-c("GCM", "Fake", "DifferentMask")

target<-targets[1]
folder<-folders[1]

for (target in targets){
  for (folder in folders){
    scs = sprintf("%s/%s", target, folder)
    scs_items<-list.dirs(scs, full.names = F, recursive = F)
    ttt<-scs_items[1]
    for (ttt in scs_items){
      r<-raster(sprintf("../coastline_direction/%s/%s.tif", folder, ttt))
      gap_threshold<-1
      
      p<-data.frame(rasterToPoints(r))
      colnames(p)[3]<-"v"
      i<-which(p$v==0)[1]
      p_v<-p
      for (i in which(p$v==0)){
        pp<-p[i,]
        #for x axis
        pp_left<-p[which((p$x==pp$x-1)&(p$y==pp$y)),]
        if (nrow(pp_left)>0){
          next()
        }
        if (p_v[i, "v"]!=0){
          next()
        }
        gap_left<-gap_threshold
        length<-1
        step<-1
        p_array<-c(as.numeric(rownames(pp)))
        while(gap_left>0){
          pp_right<-p[which((p$x==pp$x+step)&(p$y==pp$y)),]
          if (nrow(pp_right)==0){
            gap_left<-gap_left-1
            next()
          }else{
            length<-length+1
            p_array<-c(p_array, as.numeric(rownames(pp_right)))
          }
          step<-step+1
        }
        p_v[p_array, "v"]<-length
      }
      rr<-r
      values(rr)[which(!is.na(values(rr)))]<-p_v$v
      dir.create(sprintf("../coastline_direction_length_w_e/%s", folder))
      writeRaster(rr, sprintf("../coastline_direction_length_w_e/%s/%s.tif", folder, ttt), overwrite=T)
      
    }
  }
}

