setwd("C:/Users/edeegan/OneDrive - DOI/Fire_project/Fire_project/")

library(tidyverse)
library(ggrepel)
library(ordinal)
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

ggsave("C:/Users/edeegan/OneDrive - DOI/FFIanalysis/Analysis/PSME_Plots/most_common_species.png", width=10, height=4)
  

#canopy cover

#uv1 describes category
canopy=cover[which(cover$UV1=="Canopy Cover Class"),]

canopy_summary=canopy %>% group_by(Year) %>% summarize(average=mean(Cover))

canopy_summary %>% ggplot(aes(Year, average))+geom_col()+ylab("Average Canopy Cover")+theme_classic()

ggsave("C:/Users/edeegan/OneDrive - DOI/FFIanalysis/Analysis/PSME_Plots/canopy_cover_summary.png")
canopy %>% ggplot(aes(Year, Cover, fill=Year))+geom_boxplot()+geom_jitter()+theme_classic()+ylab("Canopy Cover")
ggsave("C:/Users/edeegan/OneDrive - DOI/FFIanalysis/Analysis/PSME_Plots/canopy_cover_jitter.png")
#cutting out 2004 to compare

canopy_2=canopy[-which(canopy$Year=='2004'),]
canopy_2 %>% ggplot(aes(Year, Cover, fill=Year))+geom_boxplot()+geom_jitter()+theme_classic()+ylab("Canopy Cover")
ggsave("C:/Users/edeegan/OneDrive - DOI/FFIanalysis/Analysis/PSME_Plots/canopy_cover_jitter_wo2004.png")


canopy %>% ggplot(aes(Cover, fill=MacroPlot.Name))+geom_histogram()+
  facet_wrap(~Year)+theme_classic()+
  scale_fill_manual(values=as.vector(ocean.haline(10)))
ggsave("C:/Users/edeegan/OneDrive - DOI/FFIanalysis/Analysis/PSME_Plots/canopy_cover_byplot.png")


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

canopy_species_comparison=ggarrange(stackedbarplot, canopy_graph)
ggsave("C:/Users/edeegan/OneDrive - DOI/FFIanalysis/Analysis/PSME_Plots/canopy_species_comparison.png", plot=canopy_species_comparison)

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
ggsave("C:/Users/edeegan/OneDrive - DOI/FFIanalysis/Analysis/PSME_Plots/percent_grassvforb.png")


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


#additional species and invasive species

#rereading in cover data
cover=read.csv("SAGU_data/PSME/PSME_Cover - Species Composition (metric)_XPT.csv", na.strings=c("","NA"))
#formatting date column
cover$Date=as.Date(cover$Date, format="%m/%d/%Y")
#creating new column for just year
cover$Year=str_split_i(cover$Date, "-", 1)

#filtering for additional species
#uv1 describes category
additional_species=cover[which(cover$UV1=="Additional Species"),]
additional_species=additional_species[-which(additional_species$Species.Symbol=="XXXX"),]
additional_species=additional_species %>% group_by(Year, MacroPlot.Name) %>% count()

ggplot(additional_species, aes(x=Year, y=n, fill=MacroPlot.Name))+
  geom_histogram(stat="identity")+ylab("Number of Additional Species recorded")

ggsave("C:/Users/edeegan/OneDrive - DOI/FFIanalysis/Analysis/PSME_Plots/additionalspecies.png", width=6, height=4)


#filtering for invasive species
#uv1 describes category
invasive_species=cover[which(cover$UV1=="Invasive Species"),]
invasive_species=invasive_species[-which(invasive_species$Species.Symbol=="XXXX"),]
invasive_species=invasive_species %>% group_by(Year, MacroPlot.Name) %>% count()

ggplot(invasive_species, aes(x=Year, y=n, fill=MacroPlot.Name))+
  geom_histogram(stat="identity")+ylab("Number of invasive Species recorded")

ggsave("C:/Users/edeegan/OneDrive - DOI/FFIanalysis/Analysis/PSME_Plots/invasivespecies.png", width=6, height=4)
