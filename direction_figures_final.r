setwd("~/Erin_To_Rerun/Script")

df<-read.table("../Tables/extinct_proportion_direction_removed_useless_removed.csv", head=T, sep=",", stringsAsFactors = F)
library(ggplot2)
w=1
nb="narrow"
tez="ALL"
scs<-list("Fake"=c("Eocene", "Ordovician", "Pliocene"),
          "DifferentMask"=c("Eocene", "Ordovician", "Pliocene"),
          "GCM1"=c("Alex_Eocene_Getech", "Ordovician", "Alex_Plio_Pleis"),
          "GCM2"=c("Paul_Eoc_Oligocene", "Ordovician", "Paul_Plio_Pleis"),
          "GCM3"=c("FOAM_Eoc", "Ordovician", "Paul_Plio_Pleis"),
          "GCM4"=c("FOAM_Eoc", "Ordovician", "Alex_Plio_Pleis"))
sc<-names(scs)[1]
colors<-c("#E69F00", "#56B4E9", "#009E73")
for (w in c(1,3)){
  for (nb in c("narrow", "medium")){
    for (tez in unique(df$TEZ)){
      for (sc in names(scs)){
        scx<-sc
        if (startsWith(sc, "GCM")){
          scx<-"GCM"
        }
        
        df_item<-df[which((df$edge_width==w)&(df$nb==nb)&(df$TEZ==tez)),]
        df_item<-df_item[which(df_item$direction %in% c(1, 2)),]
        df_item<-df_item[which(df_item$type==scx),]
        df_item<-df_item[which(df_item$scenario %in% scs[[sc]]),]
        if (nrow(df_item)==0){
          next
        }
        df_item$label<-paste(df_item$da, df_item$scenario_abbr)
        df_item$scenario_abbr<-factor(df_item$scenario_abbr, levels=c("Ord", "Eoc", "Plio"))
        df_item_se<-summarySE(df_item, "extinct_proportion_overall", c("direction", "da", "scenario_abbr", "label"))
        dodge_width<-0.1
        p<-ggplot(df_item_se) + 
          geom_point(aes(x=factor(direction), y=mean, 
                         shape=factor(da), color=scenario_abbr, group=factor(label)), 
                     size=3, position=position_dodge(dodge_width))+
          geom_line(aes(x=factor(direction), y=mean, 
                        color=scenario_abbr, group=factor(label)),
                    position=position_dodge(dodge_width))
        
        p<-p+ ylim(0, 0.4)+ scale_colour_manual(values = colors) +theme_bw()
        dir.create(sprintf("../Figures/direction_final/%s", sc))
        ggsave(sprintf("../Figures/direction_final/%s/%s_%d_%s.pdf", sc, nb, w, tez), p, useDingbats=FALSE)
      }
    }
  }
}



for (w in c(1,3)){
  for (nb in c("all", "narrow", "medium")){
    for (da in c("all", "Good", "Poor")){
      if ((nb!="all")&(da!="all")){
        next()
      }
      for (tez in unique(df$TEZ)){
        for (sc in names(scs)){
          scx<-sc
          if (startsWith(sc, "GCM")){
            scx<-"GCM"
          }
          
          df_item<-df[which((df$edge_width==w)),]
          if (nb!="all"){
            df_item<-df_item[which(df_item$nb==nb),]
          }
          if (da!="all"){
            df_item<-df_item[which(df_item$da==da),]
          }
          df_item<-df_item[which(df_item$direction %in% c(1, 2)),]
          df_item<-df_item[which(df_item$type==scx),]
          df_item<-df_item[which(df_item$scenario %in% scs[[sc]]),]
          if (nrow(df_item)==0){
            next
          }
          df_item$label<-paste(df_item$da, df_item$scenario_abbr)
          df_item$scenario_abbr<-factor(df_item$scenario_abbr, levels=c("Ord", "Eoc", "Plio"))
          df_item_se<-summarySE(df_item, "extinct_proportion_overall", c("direction", "da", "scenario_abbr", "label"))
          dodge_width<-0.1
          p<-ggplot(df_item_se) + 
            geom_point(aes(x=factor(direction), y=mean, color=scenario_abbr, 
                           group=factor(label)), size=3, position=position_dodge(dodge_width))+
            geom_line(aes(x=factor(direction), y=mean, 
                          color=scenario_abbr, group=factor(label)),
                      position=position_dodge(dodge_width))
          if (df_item_se[1, "mean"]!=df_item_se[1, "max"]){
            p<-p+geom_errorbar(aes(x=factor(direction), ymin=min, ymax=max,
                                   group=factor(label)), width=.1,
                               position=position_dodge(dodge_width))
          }
          p<-p+ ylim(0, 0.4)+ scale_colour_manual(values = colors) +theme_bw()
          dir.create(sprintf("../Figures/direction_final_errorbar/%s", sc))
          ggsave(sprintf("../Figures/direction_final_errorbar/%s/%s_%s_%d_%s.pdf", sc, nb, da, w, tez), p)
        }
      }
    }
  }
}

