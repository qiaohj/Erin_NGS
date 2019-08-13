setwd("~/Erin_To_Rerun/Script")
df<-read.table("../Tables/extinct_proportion_direction_removed.csv", head=T, sep=",", stringsAsFactors = F)

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

table(df$scenario_abbr)

unique(df$ttt)
write.table(df, "../Tables/extinct_proportion_direction_removed_useless_removed.csv", row.names = F, sep=",")

library(ggplot2)
df_tez_out<-df[which(df$TEZ=="out"),]

nbs<-c("all", unique(df_tez_out$nb))
das<-c("all", unique(df_tez_out$da))
combs<-expand.grid(nbs, das)
i=2
for (i in c(1:nrow(combs))){
  comb<-combs[i,]
  df_item<-df_tez_out
  if (comb$Var1!="all"){
    df_item<-df_item[which(df_item$nb==comb$Var1),]
  }
  if (comb$Var2!="all"){
    df_item<-df_item[which(df_item$da==comb$Var2),]
  }
  
  p<-ggplot(df_item) + geom_boxplot(aes(x=factor(direction), y=extinct_proportion_overall))
  ggsave(sprintf("../Figures/directions/%s_%s_overall.pdf", comb$Var1, comb$Var2), p)
  p<-ggplot(df_item) + geom_boxplot(aes(x=factor(direction), y=extinct_proportion_overall,
                                     color=factor(type)))
  ggsave(sprintf("../Figures/directions/%s_%s_scenario.pdf", comb$Var1, comb$Var2), p)
  
  p<-ggplot(df_item) + geom_boxplot(aes(x=factor(direction), y=extinct_proportion_overall,
                                     color=factor(edge_width)))
  ggsave(sprintf("../Figures/directions/%s_%s_edge_width.pdf", comb$Var1, comb$Var2), p)
  
  
  
}
sc<-unique(df$ttt)[1]

library(raster)
unique_items<-unique(df[, c("ttt", "type")])
df_all_directions<-data.frame()
for (i in c(1:nrow(unique_items))){
  scxx<-unique_items[i,]
  r<-raster(sprintf("../coastline_direction/%s/%s.tif", scxx$type, scxx$ttt))
  p<-data.frame(rasterToPoints(r))
  colnames(p)[3]<-"direction"
  
  sc<-NA
  if (grepl("Ordovician", scxx$ttt)){
    sc<-"Ordovician"
  }
  if (grepl("Eocene", scxx$ttt)){
    sc<-"Eocene"
  }
  if (grepl("_Eoc_", scxx$ttt)){
    sc<-"Eocene"
  }
  if (grepl( "Plio", scxx$ttt)){
    sc<-"Pliocene"
  }
  if (is.na(sc)){
    print(adfasfd)
  }
  
  threshold_min<-NA
  if (sc=="Pliocene"){
    threshold_min<- -62
  }
  if (sc=="Eocene"){
    threshold_min<- -64
  }
  if (sc=="Ordovician"){
    threshold_min<- -87
  }
  if (is.na(threshold_min)){
    print(adsfasdf)
  }
  p<-p[which(p$y>=threshold_min),]
  item<-data.frame(table(p$direction))
  colnames(item)[1]<-"direction"
  item$scenario<-scxx$ttt
  if (nrow(df_all_directions)==0){
    df_all_directions<-item
  }else{
    df_all_directions<-rbind(item, df_all_directions)
  }
}
df_all_directions<-unique(df_all_directions)
write.table(df_all_directions, "../Tables/N_Directions_removed.csv", row.names=F, sep=",")


p<-ggplot(df_all_directions)+geom_bar(aes(x=scenario, y=Freq, group=factor(direction), 
                                       fill=factor(direction)), stat = "identity",
                                   position = position_dodge(width = 0.9)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("../Figures/directions_removed/N_Hist_removed.pdf", p)



nbs<-c("all", unique(df$nb))
das<-c("all", unique(df$da))
tez<-c(unique(df$TEZ))
widths<-c(0, 1, 3)

scs<-list("Fake"=c("Eocene", "Ordovician", "Pliocene"),
          "DifferentMask"=c("Eocene", "Ordovician", "Pliocene"),
          "GCM1"=c("Alex_Eocene_Getech", "Ordovician", "Alex_Plio_Pleis"),
          "GCM2"=c("Paul_Eoc_Oligocene", "Ordovician", "Paul_Plio_Pleis"),
          "GCM3"=c("FOAM_Eoc", "Ordovician", "Paul_Plio_Pleis"),
          "GCM4"=c("FOAM_Eoc", "Ordovician", "Alex_Plio_Pleis"))

combs<-expand.grid(nbs, das, widths, names(scs), tez, stringsAsFactors = F)

colnames(combs)<-c("nb", "da", "width", "sc", "TEZ")
i=153
for (i in c(1:nrow(combs))){
  comb<-combs[i,]
  sc<-comb$sc
  if (startsWith(sc, "GCM")){
    sc<-"GCM"
  }
  item<-df[which(df$scenario %in% scs[[comb$sc]]),]
  item<-item[which(item$type==sc),]
  item<-item[which(item$TEZ==comb$TEZ),]
  if (comb$nb!="all"){
    item<-item[which(item$nb==comb$nb),]
  }
  if (comb$da!="all"){
    item<-item[which(item$da==comb$da),]
  }
  if (comb$width!=0){
    item<-item[which(item$edge_width==comb$width),]
  }
  item$scenario_abbr<-factor(item$scenario_abbr, levels=c("Ord", "Eoc", "Plio"))
  p<-ggplot(item) + 
    geom_boxplot(aes(x=factor(direction), y=extinct_proportion_overall, color=scenario_abbr))
  ggsave(sprintf("../Figures/direction_by_scenario_removed/%s/%s/%s_%s_%d.pdf", comb$TEZ, comb$sc, comb$nb, comb$da,
                 comb$width), p)
}


#Direction in TEZ
unique_items<-unique(df[, c("ttt", "type")])
df_all_directions<-data.frame()
for (i in c(1:nrow(unique_items))){
  scxx<-unique_items[i,]
  r<-raster(sprintf("../coastline_direction/%s/%s.tif", scxx$type, scxx$ttt))
  p<-data.frame(rasterToPoints(r))
  colnames(p)[3]<-"direction"
  for (nb in c("medium", "narrow")){
    r_TEZ<-raster(sprintf("../TEZ/%s/%s_%s.tif", scxx$type, scxx$ttt, nb))
    p$TEZ<-extract(r_TEZ, p[, c("x", "y")])
    
    sc<-NA
    if (grepl("Ordovician", scxx$ttt)){
      sc<-"Ordovician"
    }
    if (grepl("Eocene", scxx$ttt)){
      sc<-"Eocene"
    }
    if (grepl("_Eoc_", scxx$ttt)){
      sc<-"Eocene"
    }
    if (grepl( "Plio", scxx$ttt)){
      sc<-"Pliocene"
    }
    if (is.na(sc)){
      print(adfasfd)
    }
    
    threshold_min<-NA
    if (sc=="Pliocene"){
      threshold_min<- -62
    }
    if (sc=="Eocene"){
      threshold_min<- -64
    }
    if (sc=="Ordovician"){
      threshold_min<- -87
    }
    if (is.na(threshold_min)){
      print(adsfasdf)
    }
    p<-p[which(p$y>=threshold_min),]
    item<-data.frame(table(p[, c("direction", "TEZ")]))
    
    item$scenario<-scxx$ttt
    item$nb<-nb
    if (nrow(df_all_directions)==0){
      df_all_directions<-item
    }else{
      df_all_directions<-rbind(item, df_all_directions)
    }
  }
}
df_all_directions<-unique(df_all_directions)
write.table(df_all_directions, "../Tables/N_Directions_TEZ_removed.csv", row.names=F, sep=",")

for (tez in unique(df_all_directions$TEZ)){
  for (nb in c("medium", "narrow")){
    item<-df_all_directions[which((df_all_directions$TEZ==tez)&(df_all_directions$nb==nb)),]
    p<-ggplot(item)+geom_bar(aes(x=scenario, y=Freq, group=factor(direction), 
                                 fill=factor(direction)), stat = "identity",
                             position = position_dodge(width = 0.9)) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    if (tez==0){
      ggsave(sprintf("../Figures/directions_removed/N_Hist_out_TEZ_%s_removed.pdf", nb), p)
    }else{
      ggsave(sprintf("../Figures/directions_removed/N_Hist_in_TEZ_%s_removed.pdf", nb), p)
    }
  }
}
