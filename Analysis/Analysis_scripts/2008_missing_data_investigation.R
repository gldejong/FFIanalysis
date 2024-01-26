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