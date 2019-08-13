
library(stringr)
library(raster)
base<-"../20190612"

nb_v<-list("broad"=8000, "medium"=6000, "narrow"=4000)
for (base in c("../20190612", "../20190607")){
  
  
  if (base=="../20190612"){
    specieses<-c("DifferentMask/Eocene_1_5deg_Berkeley_gradient",   "DifferentMask/Eocene_3_5deg_Berkeley_gradient",   
                 "DifferentMask/Ordovician_1_5deg_Berkeley_gradient",   "DifferentMask/Ordovician_3_5deg_Berkeley_gradient",
                 "DifferentMask/Pliocene_1_5deg_Berkeley_gradient",   "DifferentMask/Pliocene_3_5deg_Berkeley_gradient",
                 
                 "Fake/Eocene_1_5deg_Berkeley_gradient",   "Fake/Eocene_3_5deg_Berkeley_gradient",   
                 "Fake/Ordovician_1_5deg_Berkeley_gradient",   "Fake/Ordovician_3_5deg_Berkeley_gradient",
                 "Fake/Pliocene_1_5deg_Berkeley_gradient",   "Fake/Pliocene_3_5deg_Berkeley_gradient"
    )
  }
  if (base=="../20190607"){
    specieses<-c("GCM/Alex_Eocene_Getech_1",
                 "GCM/Alex_Plio_Pleis_1",
                 "GCM/Paul_Eoc_Oligocene_1",
                 "GCM/Paul_Plio_Pleis_1",
                 "GCM/Alex_Eocene_Getech_3",
                 "GCM/Alex_Plio_Pleis_3",
                 "GCM/Paul_Eoc_Oligocene_3",
                 "GCM/Paul_Plio_Pleis_3",
                 "GCM/FOAM_Eoc_1", 
                 "GCM/FOAM_Eoc_3", 
                 "GCM/Ordovician_1_8x", 
                 "GCM/Ordovician_3_8x", 
                 "GCM/Ordovician_1_10x", 
                 "GCM/Ordovician_3_10x",
                 "DifferentMask/Eocene_1_10deg_Erin_gradient",  "DifferentMask/Eocene_3_10deg_Erin_gradient",  
                 "DifferentMask/Ordovician_1_10deg_Erin_gradient",  "DifferentMask/Ordovician_3_10deg_Erin_gradient",
                 "DifferentMask/Pliocene_1_10deg_Erin_gradient",  "DifferentMask/Pliocene_3_10deg_Erin_gradient",
                 "DifferentMask/Eocene_1_10deg_Qiao_gradient",  "DifferentMask/Eocene_3_10deg_Qiao_gradient",  
                 "DifferentMask/Ordovician_1_10deg_Qiao_gradient",  "DifferentMask/Ordovician_3_10deg_Qiao_gradient",
                 "DifferentMask/Pliocene_1_10deg_Qiao_gradient",  "DifferentMask/Pliocene_3_10deg_Qiao_gradient",
                 "DifferentMask/Eocene_1_5deg_Erin_gradient",   "DifferentMask/Eocene_3_5deg_Erin_gradient",   
                 "DifferentMask/Ordovician_1_5deg_Erin_gradient",   "DifferentMask/Ordovician_3_5deg_Erin_gradient",
                 "DifferentMask/Pliocene_1_5deg_Erin_gradient",   "DifferentMask/Pliocene_3_5deg_Erin_gradient",
                 "DifferentMask/Eocene_1_5deg_Qiao_gradient",   "DifferentMask/Eocene_3_5deg_Qiao_gradient",  
                 "DifferentMask/Ordovician_1_5deg_Qiao_gradient",   "DifferentMask/Ordovician_3_5deg_Qiao_gradient",
                 "DifferentMask/Pliocene_1_5deg_Qiao_gradient",   "DifferentMask/Pliocene_3_5deg_Qiao_gradient",
                 "DifferentMask/Eocene_1_6deg_Erin_gradient",   "DifferentMask/Eocene_3_6deg_Erin_gradient",   
                 "DifferentMask/Ordovician_1_6deg_Erin_gradient",   "DifferentMask/Ordovician_3_6deg_Erin_gradient",
                 "DifferentMask/Pliocene_1_6deg_Erin_gradient",   "DifferentMask/Pliocene_3_6deg_Erin_gradient",
                 "DifferentMask/Eocene_1_6deg_Qiao_gradient",   "DifferentMask/Eocene_3_6deg_Qiao_gradient",   
                 "DifferentMask/Ordovician_1_6deg_Qiao_gradient",   "DifferentMask/Ordovician_3_6deg_Qiao_gradient",
                 "DifferentMask/Pliocene_1_6deg_Qiao_gradient",   "DifferentMask/Pliocene_3_6deg_Qiao_gradient",
                 "DifferentMask/Eocene_1_8deg_Erin_gradient",   "DifferentMask/Eocene_3_8deg_Erin_gradient",   
                 "DifferentMask/Ordovician_1_8deg_Erin_gradient",   "DifferentMask/Ordovician_3_8deg_Erin_gradient",
                 "DifferentMask/Pliocene_1_8deg_Erin_gradient",   "DifferentMask/Pliocene_3_8deg_Erin_gradient",
                 "DifferentMask/Eocene_1_8deg_Qiao_gradient",   "DifferentMask/Eocene_3_8deg_Qiao_gradient",   
                 "DifferentMask/Ordovician_1_8deg_Qiao_gradient",   "DifferentMask/Ordovician_3_8deg_Qiao_gradient",
                 "DifferentMask/Pliocene_1_8deg_Qiao_gradient",   "DifferentMask/Pliocene_3_8deg_Qiao_gradient",
                 
                 "Fake/Eocene_1_10deg_Erin_gradient",  "Fake/Eocene_3_10deg_Erin_gradient",  
                 "Fake/Ordovician_1_10deg_Erin_gradient",  "Fake/Ordovician_3_10deg_Erin_gradient",
                 "Fake/Pliocene_1_10deg_Erin_gradient",  "Fake/Pliocene_3_10deg_Erin_gradient",
                 "Fake/Eocene_1_10deg_Qiao_gradient",  "Fake/Eocene_3_10deg_Qiao_gradient",  
                 "Fake/Ordovician_1_10deg_Qiao_gradient",  "Fake/Ordovician_3_10deg_Qiao_gradient",
                 "Fake/Pliocene_1_10deg_Qiao_gradient",  "Fake/Pliocene_3_10deg_Qiao_gradient",
                 "Fake/Eocene_1_5deg_Erin_gradient",   "Fake/Eocene_3_5deg_Erin_gradient",   
                 "Fake/Ordovician_1_5deg_Erin_gradient",   "Fake/Ordovician_3_5deg_Erin_gradient",
                 "Fake/Pliocene_1_5deg_Erin_gradient",   "Fake/Pliocene_3_5deg_Erin_gradient",
                 "Fake/Eocene_1_5deg_Qiao_gradient",   "Fake/Eocene_3_5deg_Qiao_gradient",  
                 "Fake/Ordovician_1_5deg_Qiao_gradient",   "Fake/Ordovician_3_5deg_Qiao_gradient",
                 "Fake/Pliocene_1_5deg_Qiao_gradient",   "Fake/Pliocene_3_5deg_Qiao_gradient",
                 "Fake/Eocene_1_6deg_Erin_gradient",   "Fake/Eocene_3_6deg_Erin_gradient",   
                 "Fake/Ordovician_1_6deg_Erin_gradient",   "Fake/Ordovician_3_6deg_Erin_gradient",
                 "Fake/Pliocene_1_6deg_Erin_gradient",   "Fake/Pliocene_3_6deg_Erin_gradient",
                 "Fake/Eocene_1_6deg_Qiao_gradient",   "Fake/Eocene_3_6deg_Qiao_gradient",   
                 "Fake/Ordovician_1_6deg_Qiao_gradient",   "Fake/Ordovician_3_6deg_Qiao_gradient",
                 "Fake/Pliocene_1_6deg_Qiao_gradient",   "Fake/Pliocene_3_6deg_Qiao_gradient",
                 "Fake/Eocene_1_8deg_Erin_gradient",   "Fake/Eocene_3_8deg_Erin_gradient",   
                 "Fake/Ordovician_1_8deg_Erin_gradient",   "Fake/Ordovician_3_8deg_Erin_gradient",
                 "Fake/Pliocene_1_8deg_Erin_gradient",   "Fake/Pliocene_3_8deg_Erin_gradient",
                 "Fake/Eocene_1_8deg_Qiao_gradient",   "Fake/Eocene_3_8deg_Qiao_gradient",   
                 "Fake/Ordovician_1_8deg_Qiao_gradient",   "Fake/Ordovician_3_8deg_Qiao_gradient",
                 "Fake/Pliocene_1_8deg_Qiao_gradient",   "Fake/Pliocene_3_8deg_Qiao_gradient"
                 
    )
  }
  for (sp in specieses){
    if (!file.exists(sprintf("%s/%s/Seeds.csv", base, sp))){
      print(sp)
    }
  }
  
  species<-specieses[1]
  for (species in specieses){
    Seeds_all<-read.table(sprintf("%s/%s/Seeds_with_nb.csv", base, species), head=F, sep=",", stringsAsFactors = F)
    colnames(Seeds_all)<-c("ID","lon","lat","v", "nb_low","nb_high","nb","da")
    
    r_seed<-raster(sprintf("%s/%s/env/1200.tif", base, species))
    r<-raster(sprintf("%s/%s/env/1199.tif", base, species))
    min_r<-min(values(r), na.rm = T)
    max_r<-max(values(r), na.rm = T)
    print(sprintf("%s/%s/env/1200.tif", base, species))
    for (nb in c("broad", "medium", "narrow")){
      nb_vv<-nb_v[[nb]]
      p_seed<-data.frame(rasterToPoints(r_seed))
      p_seed$nb_low<-p_seed$X1200-nb_vv
      p_seed$nb_high<-p_seed$X1200+nb_vv
      p_seed$extinct_type<-0
      if (nrow(p_seed[which((p_seed$nb_low>max_r)|(p_seed$nb_high<min_r)),])>0){
        p_seed[which((p_seed$nb_low>max_r)|(p_seed$nb_high<min_r)),]$extinct_type<-1
      }
      
      rr<-r_seed
      vv<-values(rr)
      vv[which(!is.na(vv))]<-p_seed$extinct_type
      values(rr)<-vv
      writeRaster(rr, sprintf("../TEZ/%s_%s.tif", species, nb), overwrite=T)
    }
  }
}