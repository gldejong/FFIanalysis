####Seedlings analysis
setwd("C:/Users/edeegan/OneDrive - DOI/FFIanalysis/Analysis/")
#load necessary packages
library(tidyverse)
library(dplyr)
library(stringr)

#clearing environment - fresh start!
rm(list = ls())

##SEEDLINGS
#read in seedling data - change 
seeds=read.csv("C:/Users/edeegan/OneDrive - DOI/Fire_project/Fire_project/SAGU_data/PSME/PSME_Trees - Seedlings (Height Class) (metric)_XPT.csv")
#see what micro plots sizes exist
unique(seeds$MicroPlotSize)

#changing microplot size unit to acres
seeds=seeds %>%
  mutate(MicroPlotSize = case_match(MicroPlotSize,
                                    0.025~0.06177635,
                                    0.005~0.01235527))


  # Identify unique events and macroplots with missing MicroPlotSize values
  events = unique(seeds[which(!is.na(seeds$MicroPlotSize)), c("MacroPlot.Name", "Monitoring.Status")])
  samp=c()
  # Iterate through each unique event-macroplot combination
  for (i in 1:nrow(events)) {
    # Find rows with the same event and macroplot as the current combination
    samp = which(events[i,"MacroPlot.Name"]==seeds[,"MacroPlot.Name"] &
                         events[i,"Monitoring.Status"] == seeds[, "Monitoring.Status"])

    # Replace missing MicroPlotSize values with values from non-missing rows within the same combination
    seeds[samp[which(is.na(seeds[samp, "MicroPlotSize"]))], "MicroPlotSize"] = seeds[which(!is.na(seeds[samp, "MicroPlotSize"])), "MicroPlotSize"]
  }



#remove na's with count for seedlings
seeds=seeds[which(!is.na(seeds$Count)),]


#formatting date column
seeds$Date_format=as.Date(seeds$Date, format="%m/%d/%Y")
#creating new column for just year
seeds$Year=str_split_i(seeds$Date_format, "-", 1)
#combining 90s into one year - unique to Saguaro
seeds[which(seeds$Year %in% c("1990", "1991", "1992")),"Year"]="1990"
#multiple by subplot fraction
seeds$Count=seeds$Count*seeds$SubFrac
#removing height class of 0
seeds=seeds[-which(seeds$SizeClHt==0),]

#counting seedlings by year size class and macroplot
seedlings_summary=seeds %>% group_by(Year, SizeClHt, MacroPlot.Name) %>% summarize(Count=sum(Count),MicroPlotSize=sum(MicroPlotSize))

#calculate density in count per acre 
seedlings_summary$density=seedlings_summary$Count/seedlings_summary$MicroPlotSize
#changing size class to character format
seedlings_summary$SizeClHt=as.character(seedlings_summary$SizeClHt)

###Size class set up and plotting

#grouping by year and macroplot and counting observations of all size classes, creating summary dataframe with density calculated
all_classes=seedlings_summary %>%
  group_by(Year, MacroPlot.Name) %>%
  summarise(Count = sum(Count),
            SizeClHt="Total",
            MicroPlotSize=sum(MicroPlotSize),
            density=Count/MicroPlotSize
            )
#adding all classes summary to seedling data frame
seedlings_summary=rbind(seedlings_summary, all_classes)

#need to take avg of plot densities for each year
seedlings_plotdata=seedlings_summary%>%
  group_by(Year, SizeClHt) %>%
  summarize(density_mean=mean(density))

#plotting mean density for size class each year and total
ggplot(seedlings_plotdata, aes(x=Year, y=density_mean,
                  group=as.factor(SizeClHt), fill=as.factor(SizeClHt)))+
  geom_vline(xintercept="2004-01-01",linetype='dashed', color='red', size=0.5)+
theme_classic()+geom_area()+labs(title="Seedling Density (tree/acre) over time",
                                 fill="Height Class")+
  scale_color_viridis_d()+
  facet_wrap(~SizeClHt)+ theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))

#saving plot
ggsave("PSME_Finalized_Plots/seedlingdensity.png", height=4, width=5)


###BY SPECIES set up and plotting





#counting seedlings by year size class, macroplot, and species
seedlings_species_summary=seeds %>% group_by(Year, SizeClHt, MacroPlot.Name, Species.Symbol) %>% summarize(Count=sum(Count),
                                                                                   MicroPlotSize=sum(MicroPlotSize))
#calculating density in count per acre
seedlings_species_summary$density=seedlings_species_summary$Count/seedlings_species_summary$MicroPlotSize
#changing size class to character format
seedlings_species_summary$SizeClHt=as.character(seedlings_species_summary$SizeClHt)



#need to take avg of plot densities for each year
seedlings_plotdata=seedlings_species_summary%>%
  group_by(Year, SizeClHt, Species.Symbol) %>%
  summarize(density_mean=mean(density))

#putting species code into common names
seedlings_plotdata=seedlings_plotdata %>%
  mutate(Species.Symbol = recode(Species.Symbol,
                                 'ABCO1' = 'White Fir',
                                 'QUGA1' = 'Gambel Oak',
                                 'PIPO1' = 'Ponderosa Pine',
                                 'PIST1' = 'Southwestern White Pine',
                                 'PSME1' = 'Douglas Fir',
                                 'QUHY1' = 'Silverleaf Oak'))

##EDITS
#cut 1997 and 2001
#add labels for years

seedlings_plotdata <- seedlings_plotdata[-which(seedlings_plotdata$Year %in% c("1990", "1997")),]
seedlings_plotdata <- seedlings_plotdata[-which(seedlings_plotdata$SizeClHt %in% c("0.15", "0.3")),]


ggplot(seedlings_plotdata, aes(x=SizeClHt, y=density_mean,
                               fill=as.factor(Species.Symbol)))+geom_bar(position='stack', stat='identity')+
  facet_grid(rows=vars(Year))+

  theme_classic()+labs(title="PSME Plots Seedling Density over time by species and size class",
                                   fill="Species")+
  scale_fill_brewer(palette = "PuOr")+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))+
  ylab("Mean Density (tree/acre)")


ggsave("PSME_Finalized_Plots/seedlingdensity_byspecies.png", height=4, width=8)



