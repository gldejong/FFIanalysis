###objective evaluation
setwd("C:/Users/edeegan/OneDrive - DOI/FFIanalysis/Analysis/")

library(tidyverse)
library(ggrepel)
library(multcompView)
#clearing environment - fresh start!
rm(list = ls())

tree=read.csv("C:/Users/edeegan/OneDrive - DOI/Fire_project/Fire_project/SAGU_data/PSME/PSME_Trees - Individuals (metric)_XPT.csv")
#formatting date column
tree$Date=as.Date(tree$Date, format="%m/%d/%Y")
#creating new column for just year
tree$Year=str_split_i(tree$Date, "-", 1)
#deleting years before 2003
tree=tree[-which(tree$Year %in% c("1990", "1991", "1992", "1997")),]
tree=tree[which(tree$Status=="L"),]

#reduce pole size tree density by 30-50% two years post burn
poles=tree[which(tree$DBH<=15.1),]

poles=poles %>% group_by(Year, MacroPlot.Name) %>% count() %>% 
  mutate(density=n/(0.24710538*4)) 


ggplot(poles, aes(x=Year, y=density))+geom_boxplot()+
  theme_classic()+ylab("Pole tree density (stems/acre)")+
  stat_summary(fun=mean, geom="point", shape=20, size=4, color="red", fill="red")+
  stat_summary(aes(label=round(..y.., 1)),vjust=1.8, fun.y=mean, geom="text", size=3)+ 
  annotate ("rect", xmin="2000", xmax="2003", ymin=30, ymax=31, alpha=0.2, color="red", fill="red")+ 
  annotate ("rect", xmin="2003", xmax="2024", ymin=30, ymax=31, alpha=0.2, color="blue", fill="blue")+ 
  annotate ("rect", xmin="2000", xmax="2024", ymin=8.9, ymax=14.24, alpha=0.2, color="green", fill="green")

ggsave("PSME_Plots/pole_tree_density.png", width=8, height=6)
poles_mean=poles %>% group_by(Year) %>% summarise(mean_pole_density=mean(density))

poles_mean %>% mutate(percent_change=(mean_pole_density-17.8)/17.8 *100)




#limit overstory tree mortality to less than 10% five years post burn

tree=read.csv("C:/Users/edeegan/OneDrive - DOI/Fire_project/Fire_project/SAGU_data/PSME/PSME_Trees - Individuals (metric)_XPT.csv")
#formatting date column
tree$Date=as.Date(tree$Date, format="%m/%d/%Y")
#creating new column for just year
tree$Year=str_split_i(tree$Date, "-", 1)
#deleting years before 2003
tree=tree[-which(tree$Year %in% c("1990", "1991", "1992", "1997", "2008")),]

#overstory trees
overstory=tree[which(tree$DBH>15.1),]

overstory=overstory %>% group_by(Year, Status) %>% count()
overstory[which(overstory$Status=="D"), "n"]=(c(110, 252, 268, 230, 131)*-1)

ggplot(overstory)+
  geom_segment(aes(x=Year, xend=Year, y=1, yend=n, color=Status))+ theme_light()+
  geom_point(aes(x=Year, y=n, color=Status), size=4)+
  theme(
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    axis.ticks.x = element_blank()
  )+ylab("Live(+) and Dead(-) tree count")+ 
  annotate ("rect", xmin="2000", xmax="2003", ymin=500, ymax=530, alpha=0.2, color="red", fill="red")+ 
  annotate ("rect", xmin="2003", xmax="2024", ymin=500, ymax=530, alpha=0.2, color="blue", fill="blue")+ 
  annotate ("rect", xmin="2000", xmax="2024", ymin=409.5, ymax=455, alpha=0.2, color="green", fill="green")

ggsave("PSME_Plots/overstory_mortality.png", width=8, height=6)

overstory %>% mutate(mortality=(n-455)/455 *100)
