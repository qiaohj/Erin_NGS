setwd("~/Dropbox/Papers/NB_Manuscripts/Erin_Paleogeography/heatmap_persent_extinction")
rm(list=ls())

library(ggplot2)
library(scales)
library(gtable)

library(pheatmap)
library(RColorBrewer)
library(vcd)
library(colorspace)

#read the tables
fake<-read.table("20190612/Fake_Berkeley_PercentExtinction_removed.csv", head=T, sep=",", stringsAsFactors = F)
colnames(fake)[4:5]<-c("Narrow_Fake", "Broad_Fake")

differentmask<-read.table("20190612/DifferentMask_Berkeley_PercentExtinction_removed.csv", head=T, sep=",", stringsAsFactors = F)
colnames(differentmask)[4:5]<-c("Narrow_DifferentMask", "Broad_DifferentMask")

gcm<-read.table("20190607/GCM_PercentExtinction_table_3_removed.csv", head=T, sep=",", stringsAsFactors = F)
gcm<-gcm[which((gcm$NB!="Broad NB")&(gcm$DA!="Moderate Dispersal")),]
colnames(gcm)[4:5]<-c("Narrow_GCM", "Broad_GCM")

#merge the tables
heat_df<-merge(merge(fake, differentmask, by=c("NB", "DA", "SC", "label")), gcm, , by=c("NB", "DA", "SC", "label"))
heat_df$NB<-factor(heat_df$NB, levels=c("Intermediate NB", "Narrow NB"))
heat_df$DA<-factor(heat_df$DA, levels=c("Good Dispersal", "Poor Dispersal"))
heat_df$SC<-factor(heat_df$SC, levels=c("Ord", "Eoc", "Plio"))
heat_df<-heat_df[order(heat_df[, "NB"], heat_df[, "DA"], heat_df[, "SC"]),]

#select the Narrow shelf only
heat_df<-heat_df[,c("NB", "DA", "SC", "label", "Narrow_Fake", "Narrow_DifferentMask", "Narrow_GCM" )]
#Here to define the annotation colors. If you change the labels in CSV file or at the beginning of the R script,
#you need to change the labels here to adjust the colors of the annotation

annotation_colors <- list(
  D1 = c("Narrow_Fake"="purple", "Narrow_DifferentMask"="yellow", "Narrow_GCM"="red"),
  D2 = c("5 temp drop"="green"),
  NB = c("Intermediate NB"="light blue", "Narrow NB"="pink"),
  DA = c("Good Dispersal"="red", "Poor Dispersal"="green"),
  SC = c("Ord"="blue", "Eoc"="green", "Plio"="red")
)               

#Here is used to change the colors of the boxes.
cols <- colorRampPalette(diverge_hcl(12, c=50, l=c(50, 90), power=1))
cols<-colorRampPalette(c("white", "red"))
boxcolors<- cols(50)

heat_df_col<-data.frame(D1=c("Narrow_Fake", "Narrow_DifferentMask", "Narrow_GCM"), 
                        D2=c("5 temp drop"))

rownames(heat_df_col)<-colnames(heat_df)[c(5, 6, 7)]


rownames(heat_df)<-heat_df$label

pheatmap(heat_df[,c(5, 6, 7)], cluster_cols = F, cluster_rows = F, color=boxcolors,
         annotation_row = heat_df[,c(3:1)], annotation_col=heat_df_col[,c(2,1)],
         annotation_colors = annotation_colors,
         annotation_names_row=F, annotation_names_col=F,
         show_rownames=F, show_colnames=F, display_numbers=T, gaps_row=F, border_color=NA,
         cellwidth=80, cellheight=20,
         filename="combined_heat_table_figure.pdf")

