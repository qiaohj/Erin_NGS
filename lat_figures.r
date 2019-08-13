setwd("~/Erin_To_Rerun/Script")
df<-read.table("../Tables/extinct_proportion_lat_removed.csv", head=T, sep=",", stringsAsFactors = F)

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
write.table(df, "../Tables/extinct_proportion_lat_removed_useless_removed.csv", row.names = F, sep=",")

library(ggplot2)

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
colors<-c("#E69F00", "#56B4E9", "#009E73")
for (i in c(1:nrow(combs))){
  print(paste(i, nrow(combs)))
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
  item_se<-summarySE(item, "extinct_proportion_overall", c("y", "scenario_abbr"))
  if (nrow(item_se)>0){
    p<-ggplot(item_se) + 
      geom_ribbon(aes(x = y, ymax = max, ymin = min, fill = scenario_abbr), alpha = 0.2) +
      geom_line(aes(x=y, y=mean, color=scenario_abbr))+
      ylim(0, 1)+scale_colour_manual(values = colors) + scale_fill_manual(values = colors) + theme_bw()
      
    ggsave(sprintf("../Figures/lat_by_scenario_removed/%s/%s/%s_%s_%d.pdf", comb$TEZ, comb$sc, comb$nb, comb$da,
                   comb$width), p)
  }
}

