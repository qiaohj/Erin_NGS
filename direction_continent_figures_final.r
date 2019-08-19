setwd("~/Erin_To_Rerun/Script/Erim_NGS")

df<-read.table("../../Tables/extinct_proportion_continent_removed.csv", head=T, sep=",", stringsAsFactors = F)

#remove the useless records
df<-df[which(!grepl("_Qiao_", df$ttt)),]
df<-df[which(!grepl("_Erin_", df$ttt)),]
df<-df[which(!grepl("_10x", df$ttt)),]
df<-df[which(df$nb!="broad"),]
df<-df[which(df$da!="Moderate"),]
df$edge_width<-1
df[which(grepl("_3", df$ttt)), "edge_width"]<-3
head(df)
df$scenario<-NA

df[which(grepl("Eocene_", df$ttt)), "scenario"]<-"Eocene"
df[which(grepl("Ordovician_", df$ttt)), "scenario"]<-"Ordovician"
df[which(grepl("Pliocene_", df$ttt)), "scenario"]<-"Pliocene"
df[which(grepl("Alex_Eocene_Getech", df$ttt)), "scenario"]<-"Alex_Eocene_Getech"
df[which(grepl("Alex_Plio_Pleis", df$ttt)), "scenario"]<-"Alex_Plio_Pleis"
df[which(grepl("FOAM_Eoc", df$ttt)), "scenario"]<-"FOAM_Eoc"
df[which(grepl("Paul_Eoc_Oligocene", df$ttt)), "scenario"]<-"Paul_Eoc_Oligocene"
df[which(grepl("Paul_Plio_Pleis", df$ttt)), "scenario"]<-"Paul_Plio_Pleis"

df$scenario_abbr<-NA

df[which(grepl("Eocene_", df$ttt)), "scenario_abbr"]<-"Eoc"
df[which(grepl("Ordovician_", df$ttt)), "scenario_abbr"]<-"Ord"
df[which(grepl("Pliocene_", df$ttt)), "scenario_abbr"]<-"Plio"
df[which(grepl("Alex_Eocene_Getech", df$ttt)), "scenario_abbr"]<-"Eoc"
df[which(grepl("Alex_Plio_Pleis", df$ttt)), "scenario_abbr"]<-"Plio"
df[which(grepl("FOAM_Eoc", df$ttt)), "scenario_abbr"]<-"Eoc"
df[which(grepl("Paul_Eoc_Oligocene", df$ttt)), "scenario_abbr"]<-"Eoc"
df[which(grepl("Paul_Plio_Pleis", df$ttt)), "scenario_abbr"]<-"Plio"


library(ggplot2)
w=1
nb="narrow"
tez="ALL"
scs<-list("Fake"=c("Ordovician"),
          "DifferentMask"=c("Ordovician"),
          "GCM"=c("Ordovician"))
sc<-names(scs)[1]
colors<-c("#E69F00", "#56B4E9", "#009E73")

for (nb in c("all", "narrow", "medium")){
  for (tez in unique(df$TEZ)){
    for (sc in names(scs)){
      scx<-sc
      if (startsWith(sc, "GCM")){
        scx<-"GCM"
      }
      df_item<-df
      if (nb!="all"){
        df_item<-df_item[which((df$nb==nb)),]
      }
      df_item<-df_item[which((df$TEZ==tez)),]
      df_item<-df_item[which(df_item$type==scx),]
      df_item<-df_item[which(df_item$scenario %in% scs[[sc]]),]
      if (nrow(df_item)==0){
        next()
      }
      df_item$label<-paste(df_item$da, df_item$scenario_abbr)
      df_item_se<-summarySE(df_item, "extinct_proportion_overall", c("group", "da", "scenario_abbr", "label"))
      
      dodge_width<-0.1
      p<-ggplot(df_item_se) + 
        geom_point(aes(x=factor(group), y=mean, 
                       shape=factor(da), color=factor(da), group=factor(da)), 
                   size=3, position=position_dodge(dodge_width))
      #+geom_line(aes(x=factor(group), y=mean, 
      #                color=factor(da), group=factor(da)),
      #            position=position_dodge(dodge_width))
      
      p<-p+ scale_colour_manual(values = colors) +theme_bw()
      #p
      dir.create(sprintf("../../Figures/direction_group_final/%s", sc))
      ggsave(sprintf("../../Figures/direction_group_final/%s/%s_%s_with_da.pdf", sc, nb, tez), p, useDingbats=FALSE)
      
      df_item_se<-summarySE(df_item, "extinct_proportion_overall", c("group", "scenario_abbr"))
      p<-ggplot(df_item_se) + 
        geom_point(aes(x=factor(group), y=mean), 
                   size=3, position=position_dodge(dodge_width))
      #+
      #  geom_line(aes(x=factor(group), y=mean),
      #            position=position_dodge(dodge_width))
      
      p<-p+ scale_colour_manual(values = colors) +theme_bw()
      #dir.create(sprintf("../Figures/direction_length_final/%s", sc))
      ggsave(sprintf("../../Figures/direction_group_final/%s/%s_%s_without_da.pdf", sc, nb, tez), p, useDingbats=FALSE)
      
    }
  }
}





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
        p<-p+ scale_colour_manual(values = colors) +theme_bw()
        dir.create(sprintf("../Figures/direction_length_final_errorbar/%s", sc))
        ggsave(sprintf("../Figures/direction_length_final_errorbar/%s/%s_%s_%d_%s.pdf", sc, nb, da, w, tez), p)
      }
    }
  }
}


