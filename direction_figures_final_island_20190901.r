setwd("~/Erin_To_Rerun/Script")
source("Erim_NGS/functions.r")


if (F){
  folder<-"../heatmap_diection_coastline_with_island_Antarctica"
  folders<-list.files(folder, recursive = T, full.names=T)
  f<-folders[1]
  fff<-data.frame()
  for (f in folders){
    print(f)
    items<-strsplit(gsub(".csv", "", f), "_")
    #items2<-strsplit(items[[1]][5], "/")
    items2<-strsplit(items[[1]][6], "/")
    df<-read.table(f, head=T, sep=",", stringsAsFactors = F)
    df$nb<-items[[1]][length(items[[1]])-1]
    df$da<-items[[1]][length(items[[1]])]
    df$type<-items2[[1]][2]
    df$ttt<-f
    
    
    if (nrow(fff)==0){
      fff<-df
    }else{
      fff<-rbind(fff, df)
    }
  }
  write.table(fff, "../Tables/extinct_proportion_island_new_20190901_Antarctica.csv", row.names = F, sep=",")
  
}

df<-read.table("../Tables/extinct_proportion_island_new_20190901_Antarctica.csv", head=T, sep=",", stringsAsFactors = F)

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
df$extinct_proportion<-df$sp_extinct/df$sp_exist
write.table(df, "../Tables/extinct_proportion_island_new_20190901_Antarctica.csv", row.names = F, sep=",")

library(ggplot2)
w=1
nb="narrow"
scs<-list("Fake"=c("Eocene", "Ordovician", "Pliocene"))
sc<-names(scs)[1]
colors<-c("#E69F00", "#56B4E9", "#009E73")

for (w in c(1)){
  for (nb in c("all", "narrow", "medium")){
    for (sc in names(scs)){
      scx<-sc
      if (startsWith(sc, "GCM")){
        scx<-"GCM"
      }
      
      df_item<-df
      if (nb!="all"){
        df_item<-df_item[which((df_item$nb==nb)),]
      }
      df_item<-df_item[which(df_item$direction %in% c(1, 2)),]
      df_item<-df_item[which(df_item$type==scx),]
      df_item<-df_item[which(df_item$scenario %in% scs[[sc]]),]
      if (nrow(df_item)==0){
        next()
      }
      df_item$label<-paste(df_item$da, df_item$scenario_abbr)
      df_item$scenario_abbr<-factor(df_item$scenario_abbr, levels=c("Ord", "Eoc", "Plio"))
      dir.create(sprintf("../Tables/direction_final_island_20190901_Antarctica/%s", sc))
      write.table(df_item, sprintf("../Tables/direction_final_island_20190901_Antarctica/%s/%s_%d_island.csv", sc, nb, w), row.names = F, sep=",")
      
      df_item_se<-summarySE(df_item, "extinct_proportion", c("direction", "da", "scenario_abbr", "label"))
      dodge_width<-0.1
      p<-ggplot(df_item_se) + 
        geom_point(aes(x=factor(direction), y=mean, 
                       shape=factor(da), color=scenario_abbr, group=factor(label)), 
                   size=3, position=position_dodge(dodge_width))+
        geom_line(aes(x=factor(direction), y=mean, 
                      color=scenario_abbr, group=factor(label)),
                  position=position_dodge(dodge_width))
      
      p<-p+ scale_colour_manual(values = colors) +theme_bw()
      dir.create(sprintf("../Figures/direction_final_island_20190901_Antarctica/%s", sc))
      ggsave(sprintf("../Figures/direction_final_island_20190901_Antarctica/%s/%s_%d_island.pdf", sc, nb, w), p, useDingbats=FALSE)
    }
  }
}



