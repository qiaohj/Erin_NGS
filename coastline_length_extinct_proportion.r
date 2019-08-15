setwd("~/Erin_To_Rerun/Script/Erim_NGS")
library("raster")

source("functions.r")



getResult<-function(p_all, group, TEZ, dii, nb, da, filename, folder){
  sp_extinct_length<-summarySE(p_all, "sp_extinct", group)
  colnames(sp_extinct_length)[3:ncol(sp_extinct_length)]<-
    paste("extinct", colnames(sp_extinct_length)[3:ncol(sp_extinct_length)], sep="_")
  sp_exist_length<-summarySE(p_all, "sp_exist", group)
  colnames(sp_exist_length)[3:ncol(sp_exist_length)]<-
    paste("species", colnames(sp_exist_length)[3:ncol(sp_exist_length)], sep="_")
  extinction_proportion_length<-summarySE(p_all, "extinction_proportion", group)
  colnames(extinction_proportion_length)[3:ncol(extinction_proportion_length)]<-
    paste("extinct_proportion", colnames(extinction_proportion_length)[3:ncol(extinction_proportion_length)], sep="_")
  
  item<-merge(sp_extinct_length, sp_exist_length, by=c(group, "N"), all=T)
  item<-merge(item, extinction_proportion_length, by=c(group, "N"), all=T)
  
  item$extinct_proportion_overall<-item$extinct_mean/item$species_mean
  item$nb<-nb
  item$da<-da
  item$type<-folder
  item$ttt<-filename
  item$TEZ<-TEZ
  item$direction<-dii
  return(item)
}

targets<-c("../../20190607", "../../20190612")
folders<-c("GCM", "Fake", "DifferentMask")

target<-targets[1]
folder<-folders[1]

df_result_length<-data.frame()
df_result_lat<-data.frame()
for (folder in folders){
  dii<-"w_e"
  for (dii in c("n_s", "w_e")){
    scs <- sprintf("../../coastline_direction_length_%s/%s", dii, folder)
    scs_items<-list.files(scs, full.names = F, pattern = "\\.tif$")
    ttt<-scs_items[1]
    for (ttt in scs_items){
      filename<-tools::file_path_sans_ext(ttt)
      if (grepl("_3", filename)){
        next()
      }
      if (grepl("Erin", filename)){
        next()
      }
      if (grepl("Qiao", filename)){
        next()
      }
      sc<-NA
      if (grepl("Ordovician", filename)){
        sc<-"Ordovician"
      }
      if (grepl("Eocene", filename)){
        sc<-"Eocene"
      }
      if (grepl("_Eoc_", filename)){
        sc<-"Eocene"
      }
      if (grepl( "Plio", filename)){
        sc<-"Pliocene"
      }
      if (is.na(sc)){
        print(adfasfd)
      }
      
      
      length<-raster(sprintf("%s/%s", scs, ttt))
      #plot(length)
      p_all_t<-data.frame(rasterToPoints(length))
      colnames(p_all_t)[3]<-"length"
      nb<-"broad"
      da<-"Good"
      print(paste(scs, ttt))
      for (nb in c("medium", "narrow")){
        TEZ<-raster(sprintf("../../TEZ/%s/%s_%s.tif",  folder, filename, nb))
        for (da in c("Good", "Poor")){
          p_all<-p_all_t
          sp_extinct_file<-sprintf("%s/heatmap/%s/%s/sp_extinct_%s_%s.tif", targets[1], folder, filename, nb, da)
          sp_exist_file<-sprintf("%s/heatmap/%s/%s/sp_exist_%s_%s.tif", targets[1], folder, filename, nb, da)
          if (!file.exists(sp_extinct_file)){
            sp_extinct_file<-sprintf("%s/heatmap/%s/%s/sp_extinct_%s_%s.tif", targets[2], folder, filename, nb, da)
            sp_exist_file<-sprintf("%s/heatmap/%s/%s/sp_exist_%s_%s.tif", targets[2], folder, filename, nb, da)
          }
          sp_extinct<-raster(sp_extinct_file)
          sp_exist<-raster(sp_exist_file)
          
          p_all$TEZ<-extract(TEZ, p_all[, c("x", "y")])
          p_all$sp_extinct<-extract(sp_extinct, p_all[, c("x", "y")])
          p_all$sp_exist<-extract(sp_exist, p_all[, c("x", "y")])
          p_all$extinction_proportion<-p_all$sp_extinct/p_all$sp_exist
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
          p_all<-p_all[which(p_all$y>=threshold_min),]
          item<-getResult(p_all, "length", "ALL", dii, nb, da, filename, folder)
          
          if (nrow(p_all[which(p_all$TEZ==0),])>0){
            item_temp<-getResult(p_all[which(p_all$TEZ==0),], "length", "out", dii, nb, da, filename, folder)
            item<-rbind(item, item_temp)
          }
          
          if (nrow(p_all[which(p_all$TEZ==1),])>0){
            item_temp<-getResult(p_all[which(p_all$TEZ==1),], "length", "in", dii, nb, da, filename, folder)
            item<-rbind(item, item_temp)
          }
          
          if (nrow(df_result_length)==0){
            df_result_length<-item
          }else{
            df_result_length<-rbind(df_result_length, item)
          }
          
          
        }
        
      }
      
    }
  }
}

write.table(df_result_length, "../../Tables/extinct_proportion_length_removed.csv", row.names=F, sep=",")

