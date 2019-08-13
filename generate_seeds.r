setwd("~/Erin_To_Rerun/Script")
library(raster)
base<-"../20190813"
dirs<-list.dirs(base)
dir<-dirs[9]
threshold_max<-100
result<-data.frame()
for (dir in dirs){
  if (str_count(dir,"/")==6){
    print(dir)
    threshold_min<-(-100)
    if (grepl("Pliocene", dir)){
      threshold_min<- -62
    }
    if (grepl("Eocene", dir)){
      threshold_min<- -64
    }
    if (grepl("Ordovician", dir)){
      threshold_min<- -87
    }
    if (grepl("_1_", dir)|(dir %in% c("/home/huijieqiao/Erin_To_Rerun/20190607/GCM/Alex_Eocene_Getech_1",
                                      "/home/huijieqiao/Erin_To_Rerun/20190607/GCM/Alex_Plio_Pleis_1",
                                      "/home/huijieqiao/Erin_To_Rerun/20190607/GCM/Paul_Eoc_Oligocene_1",
                                      "/home/huijieqiao/Erin_To_Rerun/20190607/GCM/Paul_Plio_Pleis_1"))){
      r<-raster(sprintf("%s/001.tif", dir))
      p<-data.frame(rasterToPoints(r))
      #print(dim(p))
      p<-cbind(ID = rownames(p), p)
      write.table(p, sprintf("%s/Seeds.csv", dir), row.name=F, sep=",")
      full_seed<-nrow(p)
      remove_seed<-nrow(p[which((p$y<=threshold_max)&(p$y>=threshold_min)),])
      item<-data.frame(layer=gsub("/home/huijieqiao/Erin_To_Rerun/20190607/", "", dir), full=full_seed, removed=remove_seed)
      if (nrow(result)==0){
        result<-item
      }else{
        result<-rbind(result, item)
      }
    }else{
      if (dir %in% c("/home/huijieqiao/Erin_To_Rerun/20190607/GCM/Alex_Eocene_Getech_3",
                     "/home/huijieqiao/Erin_To_Rerun/20190607/GCM/Alex_Plio_Pleis_3",
                     "/home/huijieqiao/Erin_To_Rerun/20190607/GCM/Paul_Eoc_Oligocene_3",
                     "/home/huijieqiao/Erin_To_Rerun/20190607/GCM/Paul_Plio_Pleis_3")){
        file.copy(sprintf("%s/Seeds.csv", gsub("_3", "_1", dir)), 
                  sprintf("%s/Seeds.csv", dir),
                  overwrite=T)
      }else{
        file.copy(sprintf("%s/Seeds.csv", gsub("_3_", "_1_", dir)), 
                sprintf("%s/Seeds.csv", dir),
                overwrite=T)
      }
    }
  }
}

write.table(result, "Seed_stat.csv", row.names=F, sep=",")
