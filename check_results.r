library(raster)
setwd("/home/huijieqiao/Erin_To_Rerun/20190814/DifferentMask/Ord/results/scenario.seed_4032.dispersal_Poor.nb_medium")
mask1<-raster("/home/huijieqiao/Erin_To_Rerun/20190814/DifferentMask/Ord/env/1200.tif")
mask2<-raster("/home/huijieqiao/Erin_To_Rerun/20190814/DifferentMask/Ord/env/1199.tif")


i=100
for (i in seq(from=100, to=8000, by=100)){
  
  

  
  if (i<=4000){
    plot(mask1)
  }else{
    
    plot(mask2)
  }
  
  df<-read.table(sprintf("Map_Folder/%d.csv", i-1), head=T, sep=",") 
  print(dim(df))
  points(x=df$lon, y=df$lat, col="red", pch=".")
  df<-read.table(sprintf("Map_Folder/%d.csv", i), head=T, sep=",") 
  print(dim(df))
  points(x=df$lon, y=df$lat, col="black", cex=0.15)
  
  x<-readline(prompt=sprintf("%d/%d, input 1 to stop:", i, 8500))
  if (x==1){
    break();
  }
}





