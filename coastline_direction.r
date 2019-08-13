setwd("~/Erin_To_Rerun/Script")
library("raster")



targets<-c("../20190607", "../20190612")
folders<-c("GCM", "Fake", "DifferentMask")

target<-targets[1]
folder<-folders[1]



getV<-function(p_matrix, x, y){
  if ((x<=0)|(x>nrow(r))|(y<=0)|(y>ncol(r))){
    return(NA)
  }else{
    return(p_matrix[x, y])
  }
}

for (target in targets){
  for (folder in folders){
    scs = sprintf("%s/%s", target, folder)
    scs_items<-list.dirs(scs, full.names = F, recursive = F)
    ttt<-scs_items[1]
    for (ttt in scs_items){
      r<-raster(sprintf("%s/%s/env/1200.tif", scs, ttt))
      
      
      x=1
      y=1
      p_matrix<-raster::as.matrix(r)
      
      for (x in c(1:nrow(r))){
        for (y in c(1:ncol(r))){
          #print(paste(x, y))
          v<-p_matrix[x, y]
          if (!is.na(v)){
            v_top<-getV(p_matrix, x, y-1)
            v_bottom<-getV(p_matrix, x, y+1)
            v_left<-getV(p_matrix, x-1, y)
            v_right<-getV(p_matrix, x+1, y)
            direction<-0
            all_neighbers<-0
            if (!is.na(v_top)){
              all_neighbers<-all_neighbers+1
            }
            if (!is.na(v_bottom)){
              all_neighbers<-all_neighbers+1
            }
            if (!is.na(v_left)){
              all_neighbers<-all_neighbers+1
            }
            if (!is.na(v_right)){
              all_neighbers<-all_neighbers+1
            }
            if ((!is.na(v_left))&(!is.na(v_right))){
              direction<-1
            }
            if ((!is.na(v_top))&(!is.na(v_bottom))){
              direction<-2
            }
            if (all_neighbers==4){
              direction<-3
            }
            p_matrix[x, y]<-direction
          }
        }
      }
      
      rr<-r
      values(rr)<-p_matrix
      plot(rr)
      writeRaster(rr, sprintf("../coastline_direction/%s/%s.tif", folder, ttt), overwrite=T)
      
    }
  }
}

