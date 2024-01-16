setwd("C:/Users/edeegan/OneDrive - DOI/Fire_project/Fire_project/")

samp=read.csv("SAGU_data/PSME/SAGU_SampleEventReport.csv")
cover=read.csv("SAGU_data/PSME/PSME_Cover - Species Composition (metric)_XPT.csv", na.strings=c("","NA"))
fuel1000=read.csv("SAGU_data/PSME/PSME_Surface Fuels - 1000Hr_XPT.csv", na.strings=c("","NA"))
duff=read.csv("SAGU_data/PSME/PSME_Surface Fuels - Duff_Litter_XPT.csv", na.strings=c("","NA"))
fine=read.csv("SAGU_data/PSME/PSME_Surface Fuels - Fine_XPT.csv", na.strings=c("","NA"))
saps=read.csv("SAGU_data/PSME/PSME_Trees - Saplings (Diameter Class) (metric)_XPT.csv", na.strings=c("","NA"))
seeds=read.csv("SAGU_data/PSME/PSME_Trees - Seedlings (Height Class) (metric)_XPT.csv")
tree=read.csv("SAGU_data/PSME/PSME_Trees - Individuals (metric)_XPT.csv")

seeds_1=seeds[which(seeds$MacroPlot.Name=="PSME-01" & seeds$Monitoring.Status=="00PR01"),]



tree_1=tree[which(tree$MacroPlot.Name=="PSME-03" & tree$Monitoring.Status=="00PR02"),]

tree_2=tree[which(tree$TagNo=="919"),]
tree_2.5=tree[which(tree$TagNo=="950"),]

tree_1=tree_1[which(tree_1$TagNo=="999"),]

tree_3=tree[which(tree$TagNo%in% 400:456 & tree$CrwnCl=="X"), ]


#PSME 9 changing dbh
#731, 817, 508, 369, 518, 710, 735, 819, 
#535, 546, 549, 725, 763, 772, 778, 344, 693

tree_1_=tree[which(tree$MacroPlot.Name=="PSME-03"),]

tree_2=tree_1_[which(tree_1_$TagNo=="999"),]

view(tree_2)


#613 642


