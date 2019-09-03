library(raster)
library(oceanmap)
library(reshape2)

setwd("~/Erin_To_Rerun/Script/Erim_NGS")


sps<-c( "/home/huijieqiao/Erin_To_Rerun/20190612/Fake/Eocene_1_5deg_Berkeley_gradient",   
        "/home/huijieqiao/Erin_To_Rerun/20190612/Fake/Ordovician_1_5deg_Berkeley_gradient",   
        "/home/huijieqiao/Erin_To_Rerun/20190612/Fake/Pliocene_1_5deg_Berkeley_gradient"
       )
sp<-sps[1]

threshold_max<-100

args = commandArgs(trailingOnly=TRUE)

is_threshold<-as.logical(args[1])
is_threshold<-T
if (is_threshold){
  folder<-"heatmap_diection_coastline_with_island_Antarctica"
}else{
  folder<-"heatmap_diection_coastline_with_island"
}



nb<-"narrow"
da<-"Good"
for (sp in sps){
  
  
  threshold_min<- -100
  if (grepl("Pliocene", sp)){
    threshold_min<- -62
  }
  if (grepl("Eocene", sp)){
    threshold_min<- -64
  }
  if (grepl("Ordovician", sp)){
    threshold_min<- -87
  }
  
  direction<-raster(sprintf("%s.tif", gsub("/home/huijieqiao/Erin_To_Rerun/20190612/", "../../coastline_direction_with_island/", sp)))
  p_r<-data.frame(rasterToPoints(direction))
  colnames(p_r)[3]<-"direction"
  directions<-data.frame(table(p_r$direction))
  colnames(directions)<-c("direction", "direction_Freq")
  
  for (da in c("Good", "Poor")){
    for (nb in c("medium", "narrow")){
      target<-sprintf("../../%s%s.island_%s_%s.csv", folder,  gsub("/home/huijieqiao/Erin_To_Rerun/20190612", "", sp), nb, da)
      if (file.exists(target)){
        next()
      }
      write.table(NA, target, row.names=F, sep=",")  
      
      p_final<-directions
      p_final$loss<-0
      p_final$colonize<-0
      p_final$sp_extinct<-0
      p_final$seed_extinct<-0
      p_final$sp_exist<-0
      i=1
      for (i in c(1:10000)){
        year1=4000
        year2=8000
        
        print(paste(nb, da, i, sp))
        
        map_burnin<-data.frame()
        map_after_burnin<-data.frame()
        seed_file<-sprintf("%s/results/scenario.seed_%d.dispersal_%s.nb_%s/seeds.csv", sp, i, da, nb)
        if (!file.exists(seed_file)){
          next()
        }
        seeds<-read.table(seed_file,
                          head=T, sep=",")
        if (is_threshold){
          if ((seeds$lat<threshold_min)|(seeds$lat>threshold_max)){
            next()
          }
        }
        seeds$direction<-extract(direction, seeds[, c("long", "lat")])
        burnin_file<-sprintf("%s/results/scenario.seed_%d.dispersal_%s.nb_%s/Map_Folder/%d.csv", sp, i, da, nb, year1)
        if (file.exists(burnin_file)){
          map_burnin<-read.table(burnin_file,
                                 head=T, sep=",")
          map_burnin$direction<-extract(direction, map_burnin[, c("lon", "lat")])
          
        }
        after_burnin_file<-sprintf("%s/results/scenario.seed_%d.dispersal_%s.nb_%s/Map_Folder/%d.csv", sp, i, da, nb, year2)
        
        if (file.exists(after_burnin_file)){
          map_after_burnin<-read.table(after_burnin_file, head=T, sep=",")
          map_after_burnin$direction<-extract(direction, map_after_burnin[, c("lon", "lat")])
        }
        
        if ((nrow(map_burnin)==0)){
          next
        }
        
        all_p<-directions
        all_p$burnin<-0
        all_p$after_burnin<-0
        all_p$sp_extinct<-0
        all_p$seed_extinct<-0
        
        #set the pixels occupied at the end of the simulation to 1
        if (nrow(map_burnin)>0){
          all_p[which(all_p$direction %in% map_burnin$direction),]$burnin<-1
        }
        #set the pixels occupied one step after the burnin to 1
        if (nrow(map_after_burnin)>0){
          all_p[which(all_p$direction %in% map_after_burnin$direction),]$after_burnin<-1
        }else{
          #if there is no occupied pixel after the burnin, set the distribution at the end of burnin as sp_extinct
          all_p[which(all_p$direction %in% map_burnin$direction),]$sp_extinct<-1
          all_p[which(all_p$direction %in% seeds$direction),]$seed_extinct<-1
          
        }
        
        all_p$colonize<-0
        all_p$loss<-0
        
        #set all new pixel after burnin as colonize
        if (length(all_p[which((all_p$burnin==0)&(all_p$after_burnin==1)),]$colonize)>0){
          all_p[which((all_p$burnin==0)&(all_p$after_burnin==1)),]$colonize<-1
        }
        #set all disappeared pixel after burnin as loss
        if (length(all_p[which((all_p$burnin==1)&(all_p$after_burnin==0)),]$loss)>0){
          all_p[which((all_p$burnin==1)&(all_p$after_burnin==0)),]$loss<-1
        }
        
        p_final$colonize<-all_p$colonize+p_final$colonize
        p_final$loss<-all_p$loss+p_final$loss
        p_final$sp_extinct<-all_p$sp_extinct+p_final$sp_extinct
        p_final$seed_extinct<-all_p$seed_extinct+p_final$seed_extinct
        p_final$sp_exist<-all_p$burnin + p_final$sp_exist
        
      }
      
      write.table(p_final, target, row.names=F, sep=",")  
    }
  }
}





if (F){
  df<-read.table("/home/huijieqiao/Erin_To_Rerun/heatmap_lat/DifferentMask/Eocene_1_5deg_Berkeley_gradient.lat_medium_Good.csv", head=T, sep=",", stringsAsFactors = F)
  df$rat<-df$sp_extinct/df$sp_exist
  plot(df$lat, df$rat, type="l")
}