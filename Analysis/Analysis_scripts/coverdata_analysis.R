setwd("C:/Users/edeegan/OneDrive - DOI/Fire_project/Fire_project/")

library(tidyverse)
library(ggrepel)
#clearing environment - fresh start!
rm(list = ls())

cover=read.csv("SAGU_data/PSME/PSME_Cover - Species Composition (metric)_XPT.csv", na.strings=c("","NA"))


#formatting date column
cover$Date=as.Date(cover$Date, format="%m/%d/%Y")
#creating new column for just year
cover$Year=str_split_i(cover$Date, "-", 1)


#uv1 describes category
common_species=cover[which(cover$UV1=="Most Common Herbaceous Species"),]

#counting species for each year and creating new data frame for results
species_summary=common_species %>% group_by(Year) %>% count(Species.Symbol)
species_summary=species_summary[-which(species_summary$Species.Symbol=="XXXX"),]

#counting for each year for percentages
species_summary <- species_summary %>%
  group_by(Year) %>%
  mutate(total = sum(n)) %>%
  ungroup()

#calculating species percentage of total for each year
species_summary <- species_summary %>% group_by(Species.Symbol) %>%
  mutate(percent = round(n/total*100, 1))


#renaming species code to common name - CHANGE FOR LOCAL SPECIES
species_summary=species_summary %>%
  mutate(Species.Symbol = recode(Species.Symbol, 
                                 'ARLA1' = 'Sandwort', 
                                 'BRCI1' = 'Fringed Brome',
                                 'CAGE1' = 'White Mountain Sedge',
                                 'CHEI1' = 'Lipfern',
                                 'COCO1' = 'Coulters Horseweed',
                                 'KOMA1' = 'Mountain Junegrass',
                                 'OXAL1' = 'Wood Sorrel',
                                 'PTAQ1' = 'Bracken Fern',
                                 'SCPA1' = 'Figwort',
                                 'COCA1' = 'Canadian Horseweed',
                                 'COUM1' = 'Bastard Toadflax',
                                 'GNAP1' = 'Cudweed',
                                 'HODU1' = 'Mountain Spray',
                                 'PIFI1' = 'Pinyon Rice Grass',
                                 'AGHE1' = 'Fragrant Snakeroot',
                                 'CHGR1' = 'Fetid Goosefoot',
                                 'GALI1' = 'Common Bedstraw',
                                 'GRAS1' = 'Unknown Grass',
                                 'POFE1' = 'Fendler Bluegrass',
                                 'SYOR1' = 'Mountain Snowberry',
                                 'RONE1'= 'New Mexican Locust',
                                 'FORB1'= 'Unknown Forb'))


#classifying as grass or forb
species_summary=species_summary %>%
  mutate(Veg_type = recode(Species.Symbol, 
                                 'Sandwort' = 'Forb', 
                                 'Fringed Brome' = 'Grass',
                                 'White Mountain Sedge' = 'Grass',
                                 'Lipfern' = 'Forb',
                                 'Coulters Horseweed' = 'Forb',
                                 'Mountain Junegrass' = 'Grass',
                                 'Wood Sorrel' ='Forb',
                                 'Bracken Fern' = 'Forb',
                                 'Figwort'= 'Forb',
                                 'Canadian Horseweed'= 'Forb',
                                 'Bastard Toadflax'= 'Forb',
                                 'Cudweed'= 'Forb',
                                 'Mountain Spray'= 'Forb',
                                 'Pinyon Rice Grass' = 'Grass',
                                 'Fragrant Snakeroot'= 'Forb',
                                 'Fetid Goosefoot'= 'Forb',
                                 'Common Bedstraw'= 'Forb',
                                 'Unknown Grass' = 'Grass',
                                 'Fendler Bluegrass' = 'Grass',
                                 'Mountain Snowberry'= 'Forb',
                                 'New Mexican Locust'= 'Forb',
                                 'Unknown Forb'= 'Forb'))




library(pals)
ggplot(species_summary, aes(x="", y=percent, fill=Species.Symbol)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)+
  facet_wrap(~Year)+
  geom_text(aes(label = paste(percent, "%")), size=2, 
                  position = position_stack(vjust=0.5)) +
  labs(x = NULL, y = NULL, fill = NULL)+theme_bw()+scale_fill_manual(values=as.vector(alphabet(22)))+
  labs(title = "PSME plots most common herbaceous species")+
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank())


  

#canopy cover

#uv1 describes category
canopy=cover[which(cover$UV1=="Canopy Cover Class"),]

canopy_summary=canopy %>% group_by(Year) %>% summarize(average=mean(Cover))

canopy_summary %>% ggplot(aes(Year, average))+geom_col()+ylab("Average Canopy Cover")+theme_classic()


canopy_graph_2=canopy %>% ggplot(aes(Year, Cover, fill=Year))+geom_boxplot()+geom_jitter()+theme_classic()+ylab("Canopy Cover")
#cutting out 2004 to compare

canopy_2=canopy[-which(canopy$Year=='2004'),]
canopy_graph=canopy_2 %>% ggplot(aes(Year, Cover, fill=Year))+geom_boxplot()+geom_jitter()+theme_classic()+ylab("Canopy Cover")


canopy %>% ggplot(aes(Cover, fill=MacroPlot.Name))+geom_histogram()+facet_wrap(~Year)+theme_classic()+scale_fill_manual(values=as.vector(ocean.haline(10)))


anova(lm(Cover ~ factor(Year), data = canopy))



#canopy cover and forbs vs grass

species_summary=species_summary %>%
  mutate(average_canopy_cover = recode(Year, 
                           '2008'=as.numeric(canopy_summary[which(canopy_summary$Year=='2008'),"average"]),
                           '2013'=as.numeric(canopy_summary[which(canopy_summary$Year=='2013'),"average"]),
                           '2023'=as.numeric(canopy_summary[which(canopy_summary$Year=='2023'),"average"])))





# Grouped
stackedbarplot=ggplot(species_summary, aes(fill=Veg_type, x=Year)) + 
  geom_bar(position="stack", stat="count")+theme_classic()


library(egg)

ggarrange(stackedbarplot, canopy_graph)

#percentage calculation for anova
#counting for each year for percentages

grassvforb=species_summary %>% group_by(Year) %>% count(Veg_type)
grassvforb <- grassvforb %>%
  group_by(Year) %>%
  mutate(total = sum(n)) %>%
  ungroup()
#calculating percentage of total for each year
grassvforb <- grassvforb %>% group_by(Veg_type) %>%
  mutate(percent = round(n/total*100, 1))

# Grouped
stackedbarplot_percent=ggplot(grassvforb, aes(fill=Veg_type, x=Year, y=percent)) + 
  geom_bar(position="stack", stat="identity")+theme_classic()
stackedbarplot_percent


ggarrange(stackedbarplot_percent, canopy_graph)


grassvforb=grassvforb %>%
  mutate(average_canopy_cover = recode(Year, 
                                       '2008'=as.numeric(canopy_summary[which(canopy_summary$Year=='2008'),"average"]),
                                       '2013'=as.numeric(canopy_summary[which(canopy_summary$Year=='2013'),"average"]),
                                       '2023'=as.numeric(canopy_summary[which(canopy_summary$Year=='2023'),"average"])))



#kind of failed attempt
anova(lm(percent ~ average_canopy_cover + Veg_type, data=grassvforb))


grassvforb2=grassvforb[-which(grassvforb$Veg_type=="Forb"),]

anova(lm(percent ~ average_canopy_cover, data=grassvforb2))


#LIVE CROWN BASE HEIGHT DATA FROM 2023

tree=read.csv("C:/Users/edeegan/OneDrive - DOI/Fire_project/Fire_project/SAGU_data/PSME/PSME_Trees - Individuals (metric)_XPT.csv")


#livecrown=tree[-which(is.na(tree$LiCrBHt)),] - this is only 2023 and 2013 pretty much
#livec_estimates=tree[-which(is.na(tree$LaddBaseHt)),]- this is only 2023 and 2013 pretty much




