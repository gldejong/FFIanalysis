####DBH analysis

rm(list = ls())
tree=read.csv("C:/Users/edeegan/OneDrive - DOI/Fire_project/Fire_project/SAGU_data/PSME/PSME_Trees - Individuals (metric)_XPT.csv")

library(tidyverse)
library(ggforce)

tree$Date_format=as.Date(tree$Date, format="%m/%d/%Y")
library(stringr)

tree$Year=str_split_i(tree$Date_format, "-", 1)



#p.list = lapply(sort(unique(tree$MacroPlot.Name)), function(i) {
#  ggplot(tree[tree$MacroPlot.Name==i,], aes(x=Year, y=DBH, group=TagNo, color=as.factor(TagNo)))+
#    geom_line(linetype=2)+geom_point(size=1.5)+
 #   facet_wrap_paginate(~MacroPlot.Name)+
 #   theme(legend.position="none")+
  #  theme_classic()
  
#})

#p.list[[1]]



#color for increase, decrease, stay the same


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
#tag=tree[which(tree$TagNo==303),]

ggplot(tree_2008, aes(x=Status, y=DBH, color=dbh_change))+geom_point()+
  facet_wrap(~MacroPlot.Name)+theme_classic()
ggsave("PSME_Plots/2008dbh.png", width=8, height=4)

#just dbh change values
#tree=tree[which(tree$dbh_change=="same as previous year" | tree$dbh_change=="same as subsequent year" | is.na(tree$DBH)),]
#tree=tree[-which(tree$Status=="D" & !is.na(tree$DBH)),]
#tree=tree[-which(tree$CrwnCl=="BBD" | tree$CrwnCl=="DD"),]

#tree[which(is.na(tree$dbh_change)),"dbh_change"]="dbh missing"
#compare to missing dbh plot

#tree=tree %>% arrange(TagNo, Date)


ggplot(tree, aes(x=dbh_change, fill=MacroPlot.Name))+ 
  geom_bar(width=1)+facet_wrap(~Year)+
  theme(axis.text.x= element_text(angle = 90, vjust = 0.5, hjust=1), panel.background = element_rect(fill = 'white'))+
  labs(y="Number of trees")
ggsave("PSME_Plots/dbh_transfer.png", width=8, height=5)

#all in 2008
tags_to_replace=tree_2008[which(tree_2008$dbh_change=="dbh missing"), c("TagNo", "MacroPlot.Name")]
tree_2004=tree[which(tree$Year=="2004"),c("TagNo", "MacroPlot.Name", "DBH")]


dbh_to_fill=left_join(tags_to_replace, tree_2004, by=c('MacroPlot.Name'='MacroPlot.Name', 'TagNo'='TagNo'))
write.csv(dbh_to_fill, "dbh_to_fill.csv")



##histograms of size composition over years

tree=read.csv("C:/Users/edeegan/OneDrive - DOI/Fire_project/Fire_project/SAGU_data/PSME/PSME_Trees - Individuals (metric)_XPT.csv")
tree$Date_format=as.Date(tree$Date, format="%m/%d/%Y")
tree$Year=str_split_i(tree$Date_format, "-", 1)

tree=tree[which(tree$Status=="L"),]

early_ss=tree[which(tree$Year %in% c("1990", "1991", "1992")), ]
tree=tree[-which(tree$Year %in% c("1990", "1991", "1992")), ]
early_ss$Year="1992"
tree=rbind(early_ss,tree)



#labeling by size class
#pole is less than 15.1
#medium is between 15.1 and 30
#overstory is over 30

for(x in 1:nrow(tree)){
  if(is.na(tree[x, "DBH"])){
    tree[x, "SizeClass"]="NA"
  }else{
    if(tree[x, "DBH"]<=15.1){
      #pole tree
      tree[x, "SizeClass"]="Pole (<15.1)"
    }else if(15.1<tree[x, "DBH"] & tree[x, "DBH"]<30){
      #medium tree
      tree[x, "SizeClass"]="Medium (<30)"
    }else{
      #overstory tree
      tree[x, "SizeClass"]="Overstory (30<)"
    }
    
  }
}

tree=tree %>% drop_na(DBH)
tree=tree %>% mutate(DBH=round(DBH, 0))

#density calculation
tree_1=tree %>% mutate(density=1/(tree$SubFrac*0.24710538))

ggplot(tree_1, aes(x=DBH, y=density, fill=SizeClass))+
  geom_histogram(stat="identity")+
  facet_grid(~Year)+theme_classic()+ylab("Total Density (stems/acre)")+xlab("DBH")+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


#group by size class - count then do density

tree=tree %>% mutate(acres=0.24710538*tree$SubFrac) %>% group_by(Year, SizeClass, acres, MacroPlot.Name) %>% count() %>% mutate(density=n/acres)
tree=tree %>% group_by(Year, SizeClass) %>% summarize_at(vars(density), list(average_density=mean))
level_order <- c('Pole (<15.1)', 'Medium (<30)', 'Overstory (30<)') 

ggplot(tree, aes(x=factor(SizeClass, level_order), y=average_density, fill=SizeClass))+
  geom_histogram(stat="identity")+
  facet_grid(~Year)+theme_classic()+ylab("Average Density (stems/acre)")+xlab("Size Class")+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggsave("PSME_Finalized_Plots/Dead_tree_size_histogram.png", width=15, height=5)



#group by plot - count then do density

tree=tree %>% group_by(Year, SizeClass, MacroPlot.Name, SubFrac) %>% count() %>% mutate(acres=0.24710538*SubFrac) %>% mutate(density=n/acres)
level_order <- c('Pole (<15.1)', 'Medium (<30)', 'Overstory (30<)') 

ggplot(tree, aes(x=factor(MacroPlot.Name), y=density, fill=SizeClass))+
  geom_histogram(stat="identity")+
  facet_grid(~Year)+theme_classic()+ylab("Density (stems/acre)")+xlab("Size Class")+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


ggsave("PSME_Finalized_Plots/Dead_tree_size_histogram.png", width=15, height=5)

#boxplot of every year and size class combo

tree=tree %>% group_by(Year, SizeClass, MacroPlot.Name, SubFrac) %>% count() %>% 
  mutate(acres=0.24710538*SubFrac) %>% mutate(density=n/acres) %>%
  group_by(Year, SizeClass) %>% summarize(density_sum=sum(density))


tree %>% ggplot(aes(x=Year, y=density_sum))+geom_bar(stat="identity")

tree=tree %>% group_by(Year, SizeClass, MacroPlot.Name, SubFrac) %>% count() %>% 
  mutate(acres=0.24710538*SubFrac) %>% mutate(density=n/acres)

tree %>% ggplot(aes(x=Year, y=density, fill=factor(SizeClass, level=level_order)))+geom_boxplot()
                
                