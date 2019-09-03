library(raster)
library(oceanmap)
library(reshape2)

setwd("~/Erin_To_Rerun/Script/Erim_NGS")

base1<-"/home/huijieqiao/Erin_To_Rerun/20190607"
base2<-"/home/huijieqiao/Erin_To_Rerun/20190612"

sps1<-c("GCM/Alex_Eocene_Getech_1",
       "GCM/Alex_Plio_Pleis_1",
       "GCM/Paul_Eoc_Oligocene_1",
       "GCM/Paul_Plio_Pleis_1",
       "GCM/Alex_Eocene_Getech_3",
       "GCM/Alex_Plio_Pleis_3",
       "GCM/Paul_Eoc_Oligocene_3",
       "GCM/Paul_Plio_Pleis_3",
       "GCM/FOAM_Eoc_1", 
       "GCM/FOAM_Eoc_3", 
       "GCM/Ordovician_1_8x", 
       "GCM/Ordovician_3_8x"
)

sps1<-paste(base1, sps1, sep="/")

sps2<-c("/home/huijieqiao/Erin_To_Rerun/20190612/DifferentMask/Eocene_1_5deg_Berkeley_gradient",   
       "/home/huijieqiao/Erin_To_Rerun/20190612/DifferentMask/Eocene_3_5deg_Berkeley_gradient",   
       "/home/huijieqiao/Erin_To_Rerun/20190612/DifferentMask/Ordovician_1_5deg_Berkeley_gradient",  
       "/home/huijieqiao/Erin_To_Rerun/20190612/DifferentMask/Ordovician_3_5deg_Berkeley_gradient",
       "/home/huijieqiao/Erin_To_Rerun/20190612/DifferentMask/Pliocene_1_5deg_Berkeley_gradient",  
       "/home/huijieqiao/Erin_To_Rerun/20190612/DifferentMask/Pliocene_3_5deg_Berkeley_gradient",
       
       "/home/huijieqiao/Erin_To_Rerun/20190612/Fake/Eocene_1_5deg_Berkeley_gradient",   
       "/home/huijieqiao/Erin_To_Rerun/20190612/Fake/Eocene_3_5deg_Berkeley_gradient",   
       "/home/huijieqiao/Erin_To_Rerun/20190612/Fake/Ordovician_1_5deg_Berkeley_gradient",   
       "/home/huijieqiao/Erin_To_Rerun/20190612/Fake/Ordovician_3_5deg_Berkeley_gradient",
       "/home/huijieqiao/Erin_To_Rerun/20190612/Fake/Pliocene_1_5deg_Berkeley_gradient",   
       "/home/huijieqiao/Erin_To_Rerun/20190612/Fake/Pliocene_3_5deg_Berkeley_gradient"
)
sps<-c(sps2, sps1)

sp<-sps[1]

threshold_max<-100

args = commandArgs(trailingOnly=TRUE)

is_threshold<-as.logical(args[1])
is_threshold<-T
if (is_threshold){
  folder<-"heatmap_lat_Antarctica"
}else{
  folder<-"heatmap_lat"
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
  
  l1200<-raster(sprintf("%s/env/1200.tif", sp))
  lats<-data.frame(table(data.frame(rasterToPoints(l1200))$y))
  colnames(lats)<-c("lat", "lat_Freq")
  
  for (da in c("Good", "Poor")){
    for (nb in c("medium", "narrow")){
      #tez<-raster(sprintf("%s_%s.tif", gsub(base1, "", gsub(base2, "../../TEZ", sp)), nb))
      
      target<-sprintf("../../%s%s.lat_%s_%s.csv", folder, gsub(base2, "", gsub(base1, "", sp)), nb, da)
      if (file.exists(target)){
        next()
      }
      write.table(NA, target, row.names=F, sep=",")  
      
      p_final<-lats
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
        burnin_file<-sprintf("%s/results/scenario.seed_%d.dispersal_%s.nb_%s/Map_Folder/%d.csv", sp, i, da, nb, year1)
        if (file.exists(burnin_file)){
          map_burnin<-read.table(burnin_file,
                                 head=T, sep=",")
          
        }
        after_burnin_file<-sprintf("%s/results/scenario.seed_%d.dispersal_%s.nb_%s/Map_Folder/%d.csv", sp, i, da, nb, year2)
        
        if (file.exists(after_burnin_file)){
          map_after_burnin<-read.table(after_burnin_file, head=T, sep=",")
          
        }
        
        if ((nrow(map_burnin)==0)){
          next
        }
        
        all_p<-lats
        all_p$burnin<-0
        all_p$after_burnin<-0
        all_p$sp_extinct<-0
        all_p$seed_extinct<-0
        
        #set the pixels occupied at the end of the simulation to 1
        if (nrow(map_burnin)>0){
          all_p[which(all_p$lat %in% map_burnin$lat),]$burnin<-1
        }
        #set the pixels occupied one step after the burnin to 1
        if (nrow(map_after_burnin)>0){
          all_p[which(all_p$lat %in% map_after_burnin$lat),]$after_burnin<-1
        }else{
          #if there is no occupied pixel after the burnin, set the distribution at the end of burnin as sp_extinct
          all_p[which(all_p$lat %in% map_burnin$lat),]$sp_extinct<-1
          all_p[which(all_p$lat %in% seeds$lat),]$seed_extinct<-1
          
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