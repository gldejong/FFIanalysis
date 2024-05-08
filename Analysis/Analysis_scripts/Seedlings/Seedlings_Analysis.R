####Seedlings analysis

#load necessary packages
library(tidyverse)
library(dplyr)
library(stringr)

#clearing environment - fresh start!
rm(list = ls())

##SEEDLINGS
setwd("C:/Users/edeegan/OneDrive - DOI/FFIanalysis/Analysis/")
seeds=read.csv("C:/Users/edeegan/OneDrive - DOI/Fire_project/Fire_project/SAGU_data/PSME/PSME_Trees - Seedlings (Height Class) (metric)_XPT.csv")
unique(seeds$MicroPlotSize)


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

#count per acre - translate


seeds=seeds[which(!is.na(seeds$Count)),]


#export from reports and analysis - compare and double check
# 0 v null watch out
#formatting date column
seeds$Date_format=as.Date(seeds$Date, format="%m/%d/%Y")
#creating new column for just year
seeds$Year=str_split_i(seeds$Date_format, "-", 1)
#combining 90s into one year
seeds[which(seeds$Year %in% c("1990", "1991", "1992")),"Year"]="1990"
#multiple by subplot fraction
seeds$Count=seeds$Count*seeds$SubFrac
seeds=seeds[-which(seeds$SizeClHt==0),]

seedlings_summary=seeds %>% group_by(Year, SizeClHt, MacroPlot.Name) %>% summarize(Count=sum(Count),
MicroPlotSize=sum(MicroPlotSize))

seedlings_summary$density=seedlings_summary$Count/seedlings_summary$MicroPlotSize
seedlings_summary$SizeClHt=as.character(seedlings_summary$SizeClHt)

all_classes=seedlings_summary %>%
  group_by(Year, MacroPlot.Name) %>%
  summarise(Count = sum(Count),
            SizeClHt="Total",
            MicroPlotSize=sum(MicroPlotSize),
            density=Count/MicroPlotSize
            )

seedlings_summary=rbind(seedlings_summary, all_classes)

#need to take avg of plot densities for each year
seedlings_plotdata=seedlings_summary%>%
  group_by(Year, SizeClHt) %>%
  summarize(density_mean=mean(density))


ggplot(seedlings_plotdata, aes(x=Year, y=density_mean,
                  group=as.factor(SizeClHt), fill=as.factor(SizeClHt)))+
  geom_vline(xintercept="2004-01-01",linetype='dashed', color='red', size=0.5)+
theme_classic()+geom_area()+labs(title="Seedling Density (tree/acre) over time",
                                 fill="Height Class")+
  scale_color_viridis_d()+
  facet_wrap(~SizeClHt)+ theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))


ggsave("PSME_Finalized_Plots/seedlingdensity.png", height=4, width=5)


###BY SPECIES
##SEEDLINGS
setwd("C:/Users/edeegan/OneDrive - DOI/FFIanalysis/Analysis/")
seeds=read.csv("C:/Users/edeegan/OneDrive - DOI/Fire_project/Fire_project/SAGU_data/PSME/PSME_Trees - Seedlings (Height Class) (metric)_XPT.csv")
unique(seeds$MicroPlotSize)


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

#count per acre - translate


seeds=seeds[which(!is.na(seeds$Count)),]


#export from reports and analysis - compare and double check
# 0 v null watch out
#formatting date column
seeds$Date_format=as.Date(seeds$Date, format="%m/%d/%Y")
#creating new column for just year
seeds$Year=str_split_i(seeds$Date_format, "-", 1)
#combining 90s into one year
seeds[which(seeds$Year %in% c("1990", "1991", "1992")),"Year"]="1990"
#multiple by subplot fraction
seeds$Count=seeds$Count*seeds$SubFrac
seeds=seeds[-which(seeds$SizeClHt==0),]

seedlings_summary=seeds %>% group_by(Year, SizeClHt, MacroPlot.Name, Species.Symbol) %>% summarize(Count=sum(Count),
                                                                                   MicroPlotSize=sum(MicroPlotSize))

seedlings_summary$density=seedlings_summary$Count/seedlings_summary$MicroPlotSize
seedlings_summary$SizeClHt=as.character(seedlings_summary$SizeClHt)

all_classes=seedlings_summary %>%
  group_by(Year, MacroPlot.Name, Species.Symbol) %>%
  summarise(Count = sum(Count),
            SizeClHt="Total",
            MicroPlotSize=sum(MicroPlotSize),
            density=Count/MicroPlotSize
  )

seedlings_summary=rbind(seedlings_summary, all_classes)

#need to take avg of plot densities for each year
seedlings_plotdata=seedlings_summary%>%
  group_by(Year, SizeClHt, Species.Symbol) %>%
  summarize(density_mean=mean(density))


seedlings_plotdata=seedlings_plotdata %>%
  mutate(Species.Symbol = recode(Species.Symbol,
                                 'ABCO1' = 'White Fir',
                                 'QUGA1' = 'Gambel Oak',
                                 'PIPO1' = 'Ponderosa Pine',
                                 'PIST1' = 'Southwestern White Pine',
                                 'PSME1' = 'Douglas Fir',
                                 'QUHY1' = 'Silverleaf Oak'))

##EDITS
#remove total
#cut 1997 and 2001
#add labels for years
#make sure title isn't cut off
seedlings_plotdata <- seedlings_plotdata[-which(seedlings_plotdata$SizeClHt=="Total"),]
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









library(emmeans)
library(lmerTest)

seedlings_summary=seedlings_summary[-which(seedlings_summary$Year%in% c("1990", "1997")),]

#fixed effects model
seedlings_summary_0.15=seedlings_summary[which(seedlings_summary$SizeClHt==0.15),]
seeds_lmer_0.15 <- lmer(log(density)~Year+ (1|MacroPlot.Name),
                       data=seedlings_summary_0.15)
seedlings_summary_0.3=seedlings_summary[which(seedlings_summary$SizeClHt==0.3),]
seeds_lmer_0.3 <- lmer(density~Year+ (1|MacroPlot.Name),
                        data=seedlings_summary_0.3)
seedlings_summary_0.6=seedlings_summary[which(seedlings_summary$SizeClHt==0.6),]
seeds_lmer_0.6 <- lmer(density~Year+ (1|MacroPlot.Name),
                        data=seedlings_summary_0.6)
seedlings_summary_1=seedlings_summary[which(seedlings_summary$SizeClHt==1),]
seeds_lmer_1 <- lmer(density~Year+ (1|MacroPlot.Name),
                        data=seedlings_summary_1)
seedlings_summary_2=seedlings_summary[which(seedlings_summary$SizeClHt==2),]
seeds_lmer_2 <- lmer(density~Year+ (1|MacroPlot.Name),
                        data=seedlings_summary_2)
seedlings_summary_3=seedlings_summary[which(seedlings_summary$SizeClHt==3),]
seeds_lmer_3 <- lmer(density~Year+ (1|MacroPlot.Name),
                        data=seedlings_summary_3)
seedlings_summary_t=seedlings_summary[which(seedlings_summary$SizeClHt=="Total"),]
seeds_lmer_t <- lmer(log(density)~Year+ (1|MacroPlot.Name),
                        data=seedlings_summary_t)


plot(seeds_lmer_t)


qqnorm(residuals(seeds_lmer_t))
qqline(residuals(seeds_lmer_t), col = "blue") #need help on this I've determined ask Cheryl



#results
anova(seeds_lmer_0.15)#significant
anova(seeds_lmer_0.3)#barely significant
anova(seeds_lmer_0.6)#not significant
anova(seeds_lmer_1)#not significant
anova(seeds_lmer_2)#not significant
anova(seeds_lmer_3)#not significant
anova(seeds_lmer_t) #significant

seeds_lmer_0.15_emm <- emmeans(seeds_lmer_0.15, ~Year)
contrast(seeds_lmer_0.15_emm, method="pairwise")
#no significance?

seeds_lmer_0.3_emm <- emmeans(seeds_lmer_0.3, ~Year)
contrast(seeds_lmer_0.3_emm, method="pairwise")
#no significance?

seeds_lmer_t_emm <- emmeans(seeds_lmer_t, ~Year)
contrast(seeds_lmer_t_emm, method="pairwise")
#every year is significantly different from year 2023 samples, nothing else
