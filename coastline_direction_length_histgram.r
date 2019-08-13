library("raster")
setwd("~/Erin_To_Rerun/Script")
library("raster")
library("ggplot2")


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
      if (grepl("_3_", ttt)){
        next()
      }
      if (grepl("Erin", ttt)){
        next()
      }
      if (grepl("Qiao", ttt)){
        next()
      }
      for (direction in c("w_e", "n_s")){
        r<-raster(sprintf("../coastline_direction_length_%s/%s/%s.tif", direction, folder, ttt))
        p<-data.frame(rasterToPoints(r))
        colnames(p)[3]<-"v"
        p<-ggplot(p)+geom_histogram(aes(x=v), bins=50)#+scale_y_log10()
        dir.create(sprintf("../coastline_direction_length_histgram/%s", folder))
        ggsave(p, file=sprintf("../coastline_direction_length_histgram/%s/%s_%s.pdf", folder, ttt, direction))
      }
    }
  }
}

