####Species composition analysis
#load necessary packages
library(tidyverse)
library(dplyr)
library(stringr)

#clearing environment - fresh start!
rm(list = ls())


#set working directory - CHANGE TO LOCAL SPECIFIC PATH
setwd("C:/Users/edeegan/OneDrive - DOI/FFIanalysis/Analysis/")
#load in tree data - CHANGE TO LOCAL SPECIFIC PATH
tree=read.csv("C:/Users/edeegan/OneDrive - DOI/Fire_project/Fire_project/SAGU_data/PSME/PSME_Trees - Individuals (metric)_XPT.csv")


#filtering for only alive trees
tree=tree[which(tree$Status=="L"),]
#taking out canopy data
tree=tree[-which(tree$Species.Symbol=="CANOPY"),]
#formatting date column
tree$Date_format=as.Date(tree$Date, format="%m/%d/%Y")
#creating new column for just year
tree$Year=str_split_i(tree$Date_format, "-", 1)
#counting species for each year and creating new data frame for results
species_summary=tree %>% group_by(Year, SubFrac) %>% count(Species.Symbol)

#combining years 1990, 1991, 1992 - SAGUARO SPECIFIC (DELETE FOR OTHER PARKS)

early_ss=species_summary[which(species_summary$Year %in% c("1990", "1991", "1992")), ]
species=unique(early_ss$Species.Symbol)
totals_p=c()
totals_o=c()
for(s in 1:length(species)){
  totals_p=c(totals_p, sum(early_ss[which(early_ss$Species.Symbol==species[s] & 
                                            early_ss$SubFrac==0.25),"n"]))
  totals_o=c(totals_o, sum(early_ss[which(early_ss$Species.Symbol==species[s] & 
                                            early_ss$SubFrac==1.00),"n"]))
}

early_ss=cbind(c(rep("1992", length(species)*2)), c(rep(0.25, length(species)), 
                                                       rep(1.00, length(species))), rep(species,2), c(totals_p, totals_o))
colnames(early_ss)=c("Year","SubFrac", "Species.Symbol", "n")
early_ss=as.data.frame(early_ss)
species_summary=species_summary[-which(species_summary$Year %in% c("1990", "1991", "1992")), ]
species_summary=rbind(early_ss, species_summary)
species_summary$Year=as.character(species_summary$Year)


#density calculation
species_summary$SubFrac=as.numeric(species_summary$SubFrac)
species_summary=species_summary %>%
  mutate(Acre = case_match(SubFrac,
                                    0.25~0.06177635,
                                    1~0.24710538,
                           0.05~0.01235527))
species_summary$n=as.numeric(species_summary$n)
species_summary <- species_summary %>%
  mutate(density = round(n/Acre, 1))


#calculating total tree density for each year - to be used in percentage calculation
species_summary$density=as.numeric(species_summary$density)
#removing count and acre data
species_summary=species_summary %>% select(-c(n, Acre))

#adding pole and overstory density
species_summary <- species_summary %>%
  group_by(Year, Species.Symbol) %>%
  mutate(density=sum(density))
#removing (at this point redundant) pole density data
species_summary=species_summary[-which(species_summary$SubFrac!=1),]
#removing subfrac data
species_summary=species_summary %>% select(-c(SubFrac))


species_summary <- species_summary %>%
  group_by(Year) %>%
  mutate(total = sum(density)) %>%
  ungroup()




#calculating species percentage of total for each year
species_summary <- species_summary %>% group_by(Species.Symbol) %>%
  mutate(percent = round(density/total*100, 1))


#renaming species code to common name - CHANGE FOR LOCAL SPECIES
species_summary=species_summary %>%
  mutate(Species.Symbol = recode(Species.Symbol, 
                                 'ABCO1' = 'White Fir', 
                                 'QUGA1' = 'Gambel Oak',
                                 'PIPO1' = 'Ponderosa Pine',
                                 'PIST1' = 'Southwestern White Pine',
                                 'PSME1' = 'Douglas Fir'))



library(ggrepel)
#pie chart to show species composition change over time
plot=ggplot(species_summary, aes(x="", y=percent, fill=Species.Symbol)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)+
  facet_wrap(~Year)+
  geom_text_repel(aes(label = paste(percent, "%")), size=2, 
            position = position_stack(vjust=0.5)) +
  labs(x = NULL, y = NULL, fill = NULL)+theme_bw()+scale_fill_brewer(palette = "PuOr")+
  labs(title = "PSME plots species composition over time")+
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank())
plot

#saving plot - CHANGE TITLE 
ggsave("PSME_Finalized_Plots/PSME_species_allsizes.png", width=7, height=5)



#pie chart to show species composition change over time - BY SIzE CLASS

tree=read.csv("C:/Users/edeegan/OneDrive - DOI/Fire_project/Fire_project/SAGU_data/PSME/PSME_Trees - Individuals (metric)_XPT.csv")
#filtering for only alive trees
tree=tree[which(tree$Status=="L"),]
#taking out canopy data
tree=tree[-which(tree$Species.Symbol=="CANOPY"),]
#formatting date column
tree$Date_format=as.Date(tree$Date, format="%m/%d/%Y")
#creating new column for just year
tree$Year=str_split_i(tree$Date_format, "-", 1)
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


#counting species for each year and creating new data frame for results
species_summary_2=tree %>% group_by(Year, SizeClass) %>% count(Species.Symbol)


#combining years 1990, 1991, 1992 - SAGUARO SPECIFIC (DELETE FOR OTHER PARKS)

early_ss=species_summary_2[which(species_summary_2$Year %in% c("1990", "1991", "1992")), ]

species=unique(early_ss$Species.Symbol)
sizeclasses=unique(early_ss$SizeClass)
totals=c()
for(s in 1:length(species)){
  df1=early_ss[which(early_ss$Species.Symbol==species[s]),]
  for(c in 1:length(sizeclasses)){
    df2=df1[which(df1$SizeClass==sizeclasses[c]),]
    totals=c(totals, sum(df2$n))
  }
}

early_ss=cbind(c(rep("1992", length(species)*3)), rep(species,3), totals, rep(sizeclasses, 5))
colnames(early_ss)=c("Year", "Species.Symbol", "n", "SizeClass")
early_ss=as.data.frame(early_ss)
species_summary_2=species_summary_2[-which(species_summary_2$Year %in% c("1990", "1991", "1992")), ]
species_summary_2=rbind(early_ss, species_summary_2)
species_summary_2$Year=as.character(species_summary_2$Year)


#calculating total tree count for each year - to be used in percentage calculation
species_summary_2$n=as.integer(species_summary_2$n)
species_summary_2 <- species_summary_2 %>%
  group_by(Year, SizeClass) %>%
  mutate(total = sum(n)) %>%
  ungroup()

#calculating species percentage of total for each year
species_summary_2 <- species_summary_2 %>%
  mutate(percent = round(n/total*100, 1))


#renaming species code to common name - CHANGE FOR LOCAL SPECIES
species_summary_2=species_summary_2 %>%
  mutate(Species.Symbol = recode(Species.Symbol, 
                                 'ABCO1' = 'White Fir', 
                                 'QUGA1' = 'Gambels Oak',
                                 'PIPO1' = 'Ponderosa Pine',
                                 'PIST1' = 'Southwestern White Pine',
                                 'PSME1' = 'Douglas Fir'))




#remove nas

species_summary_2=species_summary_2[-which(species_summary_2$SizeClass=="NA"),]
#arranging

species_summary_2=species_summary_2 %>%
  arrange(match(SizeClass,c("Overstory (30<)", "Medium (<30)", "Pole (<15.1)")), Year)

#plot by size class

plot=ggplot(species_summary_2, aes(x="", y=percent, fill=Species.Symbol)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)+
  facet_grid(factor(SizeClass, levels=c("Overstory (30<)", "Medium (<30)", "Pole (<15.1)")) ~ Year)+
  geom_text(aes(label = paste(percent, "%")), size=2,position = position_stack(vjust=0.5)) +
  labs(x = NULL, y = NULL, fill = NULL)+theme_bw()+scale_fill_brewer(palette = "PuOr")+
  labs(title = "PSME plots species composition over time")+
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank())
plot

#add total trees recorded
my_tag <- unique(species_summary_2$total)
my_tag=c(my_tag, 65)


#saving plot - CHANGE TITLE 
ggsave("PSME_Finalized_Plots/PSME_species_by_size.png", width=15, height=8)

