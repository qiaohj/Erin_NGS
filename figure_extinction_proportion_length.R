library("Hmisc")
setwd("~/Experiments/Erim_NGS/Script")
df<-read.table("../Tables/extinct_proportion_length_removed.csv", head=T, sep=",", stringsAsFactors = F)

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

nb<-c("all", unique(df$nb))
da<-c("all", unique(df$da))
TEZ<-unique(df$TEZ)
source("functions.r")

scs<-list("Fake"=c("Eocene", "Ordovician", "Pliocene"),
          "DifferentMask"=c("Eocene", "Ordovician", "Pliocene"),
          "GCM1"=c("Alex_Eocene_Getech", "Ordovician", "Alex_Plio_Pleis"),
          "GCM2"=c("Paul_Eoc_Oligocene", "Ordovician", "Paul_Plio_Pleis"),
          "GCM3"=c("FOAM_Eoc", "Ordovician", "Paul_Plio_Pleis"),
          "GCM4"=c("FOAM_Eoc", "Ordovician", "Alex_Plio_Pleis"))

combs<-expand.grid(nb, da, TEZ, names(scs), stringsAsFactors = F)
colnames(combs)<-c("nb", "da", "TEZ", "sc")
i=1
colors<-c("#E69F00", "#56B4E9", "#009E73")
df$length_bin<-cut2(df$length, cuts=seq(0, 20, by=1))
df$length_bin<-as.numeric(df$length_bin)-1
  
for (i in c(1:nrow(combs))){
  comb<-combs[i,]
  item<-df
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
  
  item$scenario_abbr<-factor(item$scenario_abbr, levels=c("Ord", "Eoc", "Plio"))
  
  
  item_se<-summarySE(item, "extinct_proportion_overall", c("length_bin", "scenario_abbr", "direction"))
  if (nrow(item_se)>0){
    p<-ggplot(item_se) + 
      geom_line(aes(x=length_bin, y=mean, color=scenario_abbr, linetype=direction))+
      scale_colour_manual(values = colors) + scale_fill_manual(values = colors) + theme_bw()
    
    p
    
    dir.create(sprintf("../Figures/Length_Extinction_Proporsion/%s", comb$TEZ))
    dir.create(sprintf("../Figures/Length_Extinction_Proporsion/%s/%s", comb$TEZ, comb$sc))
               
    ggsave(sprintf("../Figures/Length_Extinction_Proporsion/%s/%s/%s_%s.pdf", comb$TEZ, comb$sc, comb$nb, comb$da), p)
  }
  
}
