setwd("~/Erin_To_Rerun/20190814")
library(raster)
library(stringr)
base<-"."
dirs<-list.dirs(base)
dir<-dirs[9]
threshold_max<-100
result<-data.frame()
for (dir in dirs){
  if (str_count(dir,"/")==3){
    print(dir)
    threshold_min<-(-100)
    if (grepl("Plio", dir)){
      threshold_min<- -62
    }
    if (grepl("Eoc", dir)){
      threshold_min<- -64
    }
    if (grepl("Ord", dir)){
      threshold_min<- -87
    }
    r<-raster(sprintf("%s/1200.tif", dir))
    p<-data.frame(rasterToPoints(r))
    #print(dim(p))
    p<-cbind(ID = rownames(p), p)
    write.table(p, sprintf("%s/../Seeds.csv", dir), row.name=F, sep=",")
    full_seed<-nrow(p)
    remove_seed<-nrow(p[which((p$y<=threshold_max)&(p$y>=threshold_min)),])
    item<-data.frame(layer=dir, full=full_seed, removed=remove_seed)
    if (nrow(result)==0){
      result<-item
    }else{
      result<-rbind(result, item)
    }
  }
}

write.table(result, "Seed_stat.csv", row.names=F, sep=",")
