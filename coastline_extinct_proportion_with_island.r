setwd("~/Erin_To_Rerun/Script/Erim_NGS")
library("raster")

source("functions.r")

targets<-c("../../20190607", "../../20190612")
folders<-c("Fake")

target<-targets[1]
folder<-folders[1]

getResult<-function(p_all, group, TEZ){
  sp_extinct_direction<-summarySE(p_all, "sp_extinct", group)
  colnames(sp_extinct_direction)[3:ncol(sp_extinct_direction)]<-
    paste("extinct", colnames(sp_extinct_direction)[3:ncol(sp_extinct_direction)], sep="_")
  sp_exist_direction<-summarySE(p_all, "sp_exist", group)
  colnames(sp_exist_direction)[3:ncol(sp_exist_direction)]<-
    paste("species", colnames(sp_exist_direction)[3:ncol(sp_exist_direction)], sep="_")
  extinction_proportion_direction<-summarySE(p_all, "extinction_proportion", group)
  colnames(extinction_proportion_direction)[3:ncol(extinction_proportion_direction)]<-
    paste("extinct_proportion", colnames(extinction_proportion_direction)[3:ncol(extinction_proportion_direction)], sep="_")
  
  item<-merge(sp_extinct_direction, sp_exist_direction, by=c(group, "N"), all=T)
  item<-merge(item, extinction_proportion_direction, by=c(group, "N"), all=T)
  
  item$extinct_proportion_overall<-item$extinct_mean/item$species_mean
  item$nb<-nb
  item$da<-da
  item$type<-folder
  item$ttt<-filename
  item$TEZ<-TEZ
  return(item)
}
df_result_direction<-data.frame()
df_result_lat<-data.frame()
for (folder in folders){
  scs <- sprintf("../../coastline_direction/%s", folder)
  scs_items<-list.files(scs, full.names = F, pattern = "\\.tif$")
  ttt<-scs_items[1]
  for (ttt in scs_items){
    filename<-tools::file_path_sans_ext(ttt)
    sc<-NA
    if (grepl("_3", filename)){
      next()
    }
    if (grepl("Erin", filename)){
      next()
    }
    if (grepl("Qiao", filename)){
      next()
    }
    if (grepl("Ordovician", filename)){
      sc<-"Ordovician"
      island<-raster("../../island/Ord_bin.tif")
    }
    if (grepl("Eocene", filename)){
      sc<-"Eocene"
      island<-raster("../../island/Eoc_bin.tif")
    }
    if (grepl("_Eoc_", filename)){
      island<-raster("../../island/Eoc_bin.tif")
      sc<-"Eocene"
    }
    if (grepl( "Plio", filename)){
      island<-raster("../../island/Plio_bin.tif")
      sc<-"Pliocene"
    }
    if (is.na(sc)){
      print(adfasfd)
    }
    
    
    direction<-raster(sprintf("%s/%s", scs, ttt))
    p_all_t<-data.frame(rasterToPoints(direction))
    colnames(p_all_t)[3]<-"direction"
    p_all_t$island<-extract(island, p_all_t[, c("x", "y")])
    p_all_t[which(p_all_t$island==1),]$direction<-2
    values(direction)[!is.na(values(direction))]<-p_all_t$direction
    plot(direction)
    
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
        item<-getResult(p_all, "direction", "ALL")
        
        if (nrow(p_all[which(p_all$TEZ==0),])>0){
          item_temp<-getResult(p_all[which(p_all$TEZ==0),], "direction", "out")
          item<-rbind(item, item_temp)
        }
        
        if (nrow(p_all[which(p_all$TEZ==1),])>0){
          item_temp<-getResult(p_all[which(p_all$TEZ==1),], "direction", "in")
          item<-rbind(item, item_temp)
        }
        
        if (nrow(df_result_direction)==0){
          df_result_direction<-item
        }else{
          df_result_direction<-rbind(df_result_direction, item)
        }
        
        item<-getResult(p_all, "y", "ALL")
        
        if (nrow(p_all[which(p_all$TEZ==0),])>0){
          item_temp<-getResult(p_all[which(p_all$TEZ==0),], "y", "out")
          item<-rbind(item, item_temp)
        }
        
        if (nrow(p_all[which(p_all$TEZ==1),])>0){
          item_temp<-getResult(p_all[which(p_all$TEZ==1),], "y", "in")
          item<-rbind(item, item_temp)
        }
        
        if (nrow(df_result_lat)==0){
          df_result_lat<-item
        }else{
          df_result_lat<-rbind(df_result_lat, item)
        }
      }
      
    }
    
  }
}

write.table(df_result_direction, "../../Tables/extinct_proportion_direction_removed_island.csv", row.names=F, sep=",")
write.table(df_result_lat, "../../Tables/extinct_proportion_lat_removed_island.csv", row.names=F, sep=",")
