setwd("~/Erin_To_Rerun/Script/Erim_NGS")

if (F){
  folder<-"../../heatmap_lat"
  folders<-list.files(folder, recursive = T, full.names=T)
  f<-folders[1]
  fff<-data.frame()
  for (f in folders){
    print(f)
    items<-strsplit(gsub(".csv", "", f), "_")
    items2<-strsplit(items[[1]][2], "/")
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
  write.table(fff, "../../Tables/extinct_proportion_lat_new_20190901.csv", row.names = F, sep=",")
  
}
df<-read.table("../../Tables/extinct_proportion_lat_new_20190901.csv", head=T, sep=",", stringsAsFactors = F)

#remove the useless records
df$edge_width<-1
df[which(grepl("_3", df$ttt)), "edge_width"]<-3
head(df)
df$scenario<-NA
df$extinct_proportion<-df$sp_extinct/df$sp_exist
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

write.table(df, "../../Tables/extinct_proportion_lat_new_20190901.csv", row.names = F, sep=",")

table(df$scenario_abbr)


library(ggplot2)

nbs<-c("all", unique(df$nb))
das<-c("all", unique(df$da))
#tez<-c(unique(df$TEZ))
widths<-c(0, 1, 3)

scs<-list("Fake"=c("Eocene", "Ordovician", "Pliocene"),
          "DifferentMask"=c("Eocene", "Ordovician", "Pliocene"),
          "GCM1"=c("Alex_Eocene_Getech", "Ordovician", "Alex_Plio_Pleis"),
          "GCM2"=c("Paul_Eoc_Oligocene", "Ordovician", "Paul_Plio_Pleis"),
          "GCM3"=c("FOAM_Eoc", "Ordovician", "Paul_Plio_Pleis"),
          "GCM4"=c("FOAM_Eoc", "Ordovician", "Alex_Plio_Pleis"))

combs<-expand.grid(nbs, das, widths, names(scs), stringsAsFactors = F)

colnames(combs)<-c("nb", "da", "width", "sc")
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
  if (comb$nb!="all"){
    item<-item[which(item$nb==comb$nb),]
  }
  if (comb$da!="all"){
    item<-item[which(item$da==comb$da),]
  }
  if (comb$width!=0){
    item<-item[which(item$edge_width==comb$width),]
  }
  
  
  item<-item[-which((item$scenario_abbr=="Plio")&(item$lat<(-62))),]
  item<-item[-which((item$scenario_abbr=="Eoc")&(item$lat<(-64))),]
  item<-item[-which((item$scenario_abbr=="Ord")&(item$lat<(-87))),]
  
  item$scenario_abbr<-factor(item$scenario_abbr, levels=c("Ord", "Eoc", "Plio"))
  item_se<-summarySE(item, "extinct_proportion", c("lat", "scenario_abbr"))
  if (nrow(item_se)>0){
    p<-ggplot(item_se) + 
      geom_ribbon(aes(x = lat, ymax = max, ymin = min, fill = scenario_abbr), alpha = 0.2) +
      geom_line(aes(x=lat, y=mean, color=scenario_abbr))+
      ylim(0, 1)+scale_colour_manual(values = colors) + scale_fill_manual(values = colors) + theme_bw()
    
    ggsave(sprintf("../../Figures/lat_by_scenario_new_20190901/%s/%s_%s_%d.pdf", comb$sc, comb$nb, comb$da,
                   comb$width), p)
  }
}

