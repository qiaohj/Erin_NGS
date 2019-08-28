setwd("~/Erin_To_Rerun/Script/Erim_NGS")
library("raster")
library("ggplot2")
library("Hmisc")
source("functions.r")

folder<-"../../coastline_direction/Fake"
fs<-list.files(folder, full.names = F, pattern = "\\.tif$")
r<-NA
colors<-c("#E69F00", "#56B4E9", "#009E73")
f<-fs[21]
for (f in fs){
  if (grepl("_3_", f)){
    next()
  }
  if (grepl("Berkeley", f)){
    r<-raster(sprintf("%s/%s", folder, f))
    p<-data.frame(rasterToPoints(r))
    colnames(p)[3]<-"v"
    
    if (F){
      threshold_min<-NA
      
      scc<-NA
      if (grepl("Pliocene", f)){
        threshold_min<- -62
        scc<-"Pliocene"
      }
      if (grepl("Eocene", f)){
        threshold_min<- -64
        scc<-"Eocene"
      }
      if (grepl("Ordovician", f)){
        threshold_min<- -87
        scc<-"Ordovician"
      }
      if (is.na(threshold_min)){
        print(adsfasdf)
      }
      p<-p[which(p$y>=threshold_min),]
    }else{
      scc<-NA
      if (grepl("Pliocene", f)){
        scc<-"Pliocene"
      }
      if (grepl("Eocene", f)){
        scc<-"Eocene"
      }
      if (grepl("Ordovician", f)){
        scc<-"Ordovician"
      }
      if (is.na(scc)){
        print(adsfasdf)
      }
    }
    p$y_bin<-cut2(p$y, cuts=seq(-180, 180, by=5), levels.mean=T)
    
    t<-data.frame(table(p$y, p$v))
    
    colnames(t)<-c("y", "direction", "Freq")
    
    t<-t[which(t$direction %in% c(1, 2)),]
    t<-t[which(t$Freq>0),]
    
    t$y<-as.numeric(as.character(t$y))
    write.table(t, sprintf("../../Tables/direction_by_lat/%s_with_antartica.csv", scc), row.names = F, sep=",")
    p<-ggplot(t)+geom_line(aes(x=y, y=Freq, color=direction))+
      scale_colour_manual(values = colors) +theme_bw()
    ggsave(sprintf("../../Figures/direction_by_lat/%s_with_antartica.pdf", scc), p, width=8, height=5)
  }  
}


