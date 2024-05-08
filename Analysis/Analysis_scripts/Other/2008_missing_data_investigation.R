library(ggplot2)
library(tidyverse)
setwd("C:/Users/edeegan/OneDrive - DOI/FFIanalysis/Analysis/")
tree=read.csv("C:/Users/edeegan/OneDrive - DOI/Fire_project/Fire_project/SAGU_data/PSME/PSME_Trees - Individuals (metric)_XPT.csv")

#ggplot(tree, aes(x=SubFrac, y=DBH, color=MacroPlot.Name))+geom_point()+xlim(0,2)

tree1=tree[which(tree$Monitoring.Status=="01Year05"),]

tree1=tree1 %>% count(TagNo,DBH,MacroPlot.Name)

tree1$DBH=as.character(tree1$DBH)


tree1$status="Recorded_DBH"
tree1[which(is.na(tree1$DBH)),"status"]="No_DBH"

tree2=tree1 %>% count(TagNo,MacroPlot.Name, status)
tree2=tree2 %>% count(MacroPlot.Name, status)

ggplot(tree2, aes(x=status,y=n, fill=status))+ 
  geom_bar(stat="identity", width=1)+facet_wrap(~MacroPlot.Name)+
  theme(axis.text.x= element_text(angle = 90, vjust = 0.5, hjust=1), panel.background = element_rect(fill = 'white'))+
  labs(title="2008 missing dbhs", y="Number of trees")

ggsave("PSME_Finalized_Plots/2008missingdbhs.png")

tree=read.csv("C:/Users/edeegan/OneDrive - DOI/Fire_project/Fire_project/SAGU_data/PSME/PSME_Trees - Individuals (metric)_XPT.csv")

#ggplot(tree, aes(x=SubFrac, y=DBH, color=MacroPlot.Name))+geom_point()+xlim(0,2)

tree1=tree[which(tree$Monitoring.Status=="01Year05"),]

tree1=tree1 %>% count(TagNo,CrwnCl,MacroPlot.Name)

tree1$CrwnCl=as.character(tree1$CrwnCl)
tree1[which(tree1$CrwnCl==""), "CrwnCl"]=NA
tree1[which(tree1$CrwnCl=="X"), "CrwnCl"]=NA

tree1$status="Recorded_CrwnCl"
tree1[which(is.na(tree1$CrwnCl)),"status"]="No_CrwnCl"

tree2=tree1 %>% count(TagNo,MacroPlot.Name, status)
tree2=tree2 %>% count(MacroPlot.Name, status)

ggplot(tree2, aes(x=status,y=n, fill=status))+ 
  geom_bar(stat="identity", width=1)+facet_wrap(~MacroPlot.Name)+
  theme(axis.text.x= element_text(angle = 90, vjust = 0.5, hjust=1), panel.background = element_rect(fill = 'white'))+
  labs(title="2008 missing CrwnCls", y="Number of trees")

ggsave("PSME_Finalized_Plots/2008missingcrwnclass.png")


#replacing dbh code!
rm(list = ls())
tree=read.csv("C:/Users/edeegan/OneDrive - DOI/Fire_project/Fire_project/SAGU_data/PSME/PSME_Trees - Individuals (metric)_XPT.csv")

tree$Date_format=as.Date(tree$Date, format="%m/%d/%Y")

tree$Year=str_split_i(tree$Date_format, "-", 1)


library(dplyr)

years=sort(unique(tree$Year))


tree$dbh_change=NA

tree[which(tree$Year==1990), "dbh_change"]="dbh missing"
tree=tree[-which(tree$Status=="X"),]
tree=tree[-which(tree$TagNo==999),]
tree=tree[which(is.na(tree$MacroPlotSize)),]
tree=tree[-which(tree$SubFrac==1000),]

#plot and tag!
for(x in 2:length(years)){
  previous_year=years[x-1]
  current_year=years[x]
  plots=unique(tree[which(tree$Year==current_year), "MacroPlot.Name"])
  
  for(p in 1:length(plots)){
    tree_plot=tree[which(tree$MacroPlot.Name==plots[p]),]
    tree=tree[-which(tree$MacroPlot.Name==plots[p]),]
    trees_p=tree_plot[which(tree_plot$Year==previous_year),"TagNo"]
    trees_c=tree_plot[which(tree_plot$Year==current_year), "TagNo"]
    
    trees_c=intersect(trees_p, trees_c)
    
    for(i in 1:length(trees_c)){
      current_dbh=tree_plot[which(tree_plot$TagNo==trees_c[i] & tree_plot$Year==current_year), "DBH"]
      previous_dbh=tree_plot[which(tree_plot$TagNo==trees_c[i] & tree_plot$Year==previous_year), "DBH"]
      
      if(length(current_dbh)==0 | length(previous_dbh)==0){
        #do nothing?
      }else{
        
        if(length(current_dbh)>1 | length(previous_dbh)>1){
          current_dbh=unique(current_dbh)
          current_dbh=current_dbh[1]
          previous_dbh=unique(previous_dbh)
          previous_dbh=previous_dbh[1]}
        
        if(is.na(previous_dbh) | is.na(current_dbh)){
          tree_plot[which(tree_plot$TagNo==trees_c[i] & tree_plot$Year==current_year), "dbh_change"]="dbh missing"
        }else{
          
          if(previous_dbh<current_dbh){
            #increase
            tree_plot[which(tree_plot$TagNo==trees_c[i] & tree_plot$Year==current_year), "dbh_change"]="increase"
          }else if(previous_dbh>current_dbh){
            #decrease
            tree_plot[which(tree_plot$TagNo==trees_c[i] & tree_plot$Year==current_year), "dbh_change"]="decrease"
          }else if(previous_dbh==current_dbh){
            #stay same
            tree_plot[which(tree_plot$TagNo==trees_c[i] & tree_plot$Year==current_year), "dbh_change"]="same as previous year"
            tree_plot[which(tree_plot$TagNo==trees_c[i] & tree_plot$Year==previous_year), "dbh_change"]="same as subsequent year"
          }
        }
      }
      
    } 
    tree=rbind(tree, tree_plot)
  }
}


#just look at 2008
tree_2008=tree[which(tree$Year=="2008"),]


#all in 2008
tags_to_replace=tree_2008[which(tree_2008$dbh_change=="dbh missing"), c("TagNo", "MacroPlot.Name")]
tree_2004=tree[which(tree$Year=="2004"),c("TagNo", "MacroPlot.Name", "DBH")]


dbh_to_fill=left_join(tags_to_replace, tree_2004, by=c('MacroPlot.Name'='MacroPlot.Name', 'TagNo'='TagNo'))



tree_to_replace=read.csv("C:/Users/edeegan/OneDrive - DOI/FFI/Export/2008_PSME_Trees - Individuals (metric)_XPT.csv")

for(x in 1:nrow(tree_to_replace)){
  row=which(dbh_to_fill[x,"TagNo"]==tree_to_replace[, "TagNo"] & 
          dbh_to_fill[x,"MacroPlot.Name"]==tree_to_replace[, "MacroPlot.Name"])
  tree_to_replace[row, "DBH"]=dbh_to_fill[x,"DBH"]
  comment=tree_to_replace[row, "Comment"]
  tree_to_replace[row, "Comment"]=paste(comment, "DBH copied from 2004")
}

tree_to_replace = tree_to_replace %>% 
  mutate(Spp_GUID = recode(Species.Symbol, 
                                 'ABCO1' = '346adde2-6563-4083-b88d-bfb95778f806', 
                                 'QUGA1' = '92a767c9-f955-4bd4-a006-e59bbd909495',
                                 'PIPO1' = 'b6275763-9179-42e0-a5a2-e1600ea2384a',
                                 'PIST1' = '60d324f6-af0f-49be-a1e9-a64d205bf2a8',
                                 'PSME1' = '1dcfc4d2-2c4d-462f-9b1f-7705d28e5bc5',
                                 'CANOPY'='719fb3f3-30ff-4028-b955-c266cea53602'))

#order of columns needed
#fix canopy

plots=unique(tree_to_replace$MacroPlot.Name)

for(p in 1:length(plots)){
  plot=plots[p]
  csv=tree_to_replace[which(tree_to_replace$MacroPlot.Name==plot),]
  csv=csv[-1,]
  csv=csv %>% select(Index,SubFrac,QTR,TagNo,Species.Symbol,Spp_GUID,Status,CharHt,ScorchHt,CrScPct,CrwnCl,DBH,Ht,Comment,DamCd1,DamCd2,DamCd3,DamCd4,DamCd5,LaddMaxHt,LaddBaseHt,LiCrBHt,UV1,IsVerified,UV2,UV3,CrwnRto,CrFuBHt,CrwnRad,Age,GrwthRt,Mort,DecayCl,DRC,NuLiStems,NuDeStems,EqDia,XCoord,YCoord,CKR,DamSev1,DamSev2,DamSev3,DamSev4,DamSev5)
  filename=paste("C:/Users/edeegan/OneDrive - DOI/FFI/Import/2008_replacement_csvs/", plot, "_2008_Trees - Individuals (metric)_XPT.csv", sep="")
  write.csv(csv, filename, na="")
  
}

#0.1, 15.1, 0.1, Leckie, Grissom, Shay, Riggins, unknown
