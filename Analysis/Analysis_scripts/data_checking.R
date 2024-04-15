setwd("C:/Users/edeegan/OneDrive - DOI/Fire_project/Fire_project/")

library(tidyverse)
#clearing environment - fresh start!
rm(list = ls())

samp=read.csv("SAGU_data/PSME/SAGU_SampleEventReport.csv")
cover=read.csv("SAGU_data/PIPO/PIPO_Cover - Species Composition (metric)_XPT.csv", na.strings=c("","NA"))
fuel1000=read.csv("SAGU_data/PIPO/PIPO_Surface Fuels - 1000Hr_XPT.csv", na.strings=c("","NA"))
duff=read.csv("SAGU_data/PIPO/PIPO_Surface Fuels - Duff_Litter_XPT.csv", na.strings=c("","NA"))
fine=read.csv("SAGU_data/PIPO/PIPO_Surface Fuels - Fine_XPT.csv", na.strings=c("","NA"))
saps=read.csv("SAGU_data/PIPO/PIPO_Trees - Saplings (Diameter Class) (metric)_XPT.csv", na.strings=c("","NA"))
seeds=read.csv("SAGU_data/PIPO/PIPO_Trees - Seedlings (Height Class) (metric)_XPT.csv")
tree=read.csv("C:/Users/edeegan/OneDrive - DOI/Fire_project/Fire_project/SAGU_data/PSME/PSME_Trees - Individuals (metric)_XPT.csv")



tree$Date=as.Date(tree$Date, "%m/%d/%Y %H:%M:%S")

tree_1_=tree[which(tree$MacroPlot.Name=="PSME-10"),]
tree_1_=tree_1_[which(tree_1_$Monitoring.Status=="01Post"),]

tree_2=tree_1_[which(tree_1_$TagNo %in% c(999)),] 

tree_2=tree_2 %>% arrange(TagNo, Date)

View(tree_2)

tree_2=tree_1_[which(tree_1_$DamCd3=="X"),]
tree_2=tree_1_[which(tree_1_$DBH==89.1),]

fuels=fuel1000[which(fuel1000$Transect==4 & fuel1000$MacroPlot.Name=="PIPO-27"),]
fuels=fine[which(fine$Transect==4 & fine$MacroPlot.Name=="PIPO-27"),]
view(fuels)


seeds_1=seeds[which(seeds$MacroPlot.Name=="PIPO-04" & seeds$Monitoring.Status=="01Post"),]


seeds_1=seeds_1[which(seeds_1$MicroPlotSize==0),]


view(seeds_1)

duff[which(duff$MacroPlot.Name=="PIPO-06" & duff$Monitoring.Status=="02Year08"),]
unique(duff$NumTran)





#PSME pole to overstory checkout

trees=unique(tree$TagNo)
switching_tags=c()

for(x in 1:length(trees)){
  tree_x=tree[which(tree$TagNo==trees[x] & tree$Status=="D"),]
  if(length(na.omit(unique(tree_x$DBH)))<=1){
    #all good
  }else{
    dbhs=na.omit(tree_x$DBH)
    if(any(dbhs<15.1) & any(dbhs>15.1)){
      switching_tags=c(switching_tags, trees[x], "found in", unique(tree_x$MacroPlot.Name), "dbh history", dbhs, ":  ")
    }
  }

}
switching_tags=paste(switching_tags, collapse=" ")
print(switching_tags)



