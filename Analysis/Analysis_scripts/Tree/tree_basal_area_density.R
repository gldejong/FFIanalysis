setwd("C:/Users/edeegan/OneDrive - DOI/Fire_project/Fire_project/")

library(tidyverse)
library(ggrepel)
library(multcompView)
#clearing environment - fresh start!
rm(list = ls())

#reading in cover data
cover=read.csv("SAGU_data/PSME/PSME_Cover - Species Composition (metric)_XPT.csv", na.strings=c("","NA"))
#formatting date column
cover$Date=as.Date(cover$Date, format="%m/%d/%Y")
#creating new column for just year
cover$Year=str_split_i(cover$Date, "-", 1)
#uv1 describes category
canopy=cover[which(cover$UV1=="Canopy Cover Class"),]

#reading in tree data
tree=read.csv("C:/Users/edeegan/OneDrive - DOI/Fire_project/Fire_project/SAGU_data/PSME/PSME_Trees - Individuals (metric)_XPT.csv")
#formatting date column
tree$Date=as.Date(tree$Date, format="%m/%d/%Y")
#creating new column for just year
tree$Year=str_split_i(tree$Date, "-", 1)

#combining 1990, 1991, 1992
tree[which(tree$Year=='1990' | tree$Year=='1991'), "Year"]='1992'

#renaming species code to common name - CHANGE FOR LOCAL SPECIES
tree=tree %>%
  mutate(Species.Symbol = case_match(Species.Symbol, 
                                 'ABCO1' ~ 'White Fir', 
                                 'QUGA1' ~ 'Gambels Oak',
                                 'PIPO1' ~ 'Ponderosa Pine',
                                 'PIST1' ~ 'Southwestern White Pine',
                                 'PSME1' ~ 'Douglas Fir'))

#TREE BASAL AREA CALCULATION
tree_1=tree[-which(tree$Status=="D"),] #only live trees
#tree_1=tree_1[-which(tree_1$DBH<=15.1),] #remove poles
tree_1$DBH=tree_1$DBH*0.39370079 #convert to inches
tree_1$basal_area=(tree_1$DBH^2)*0.005454 #formula
tree_1$basal_area=as.numeric(tree_1$basal_area)
tree_1=tree_1[-which(is.na(tree_1$basal_area)),] #only tree w basal area

#sum basal area by year and plot
tree_basal=tree_1 %>% group_by(Year, MacroPlot.Name, SubFrac, Species.Symbol) %>% summarize(total_ba=sum(basal_area))
#adding acres
tree_basal=tree_basal %>%
  mutate(acres = 0.24710538*SubFrac) %>% mutate(density=total_ba/acres)
#sum density by plot and year
tree_basal=tree_basal %>% group_by(Year, MacroPlot.Name, Species.Symbol) %>% summarize(totalba_peracre=sum(density))

##OPTIONAL FILTER FOR STAND REPLACING PLOTS
tree_basal_2=tree_basal %>% filter(MacroPlot.Name=="PSME-03" | MacroPlot.Name=="PSME-07"|
                        MacroPlot.Name=="PSME-08"|MacroPlot.Name=="PSME-09")

write.csv(tree_basal_2, "tree_basal.csv")

#averaging/plot by year
tree_basal_summary=tree_basal_2 %>% group_by(Year, Species.Symbol) %>% summarize(avg_plot_ba=mean(totalba_peracre))



#plotting by species
plot=ggplot(tree_basal_2, aes(x=Year, y=totalba_peracre, fill=Species.Symbol))+
  geom_bar(position="stack", stat="identity")+theme_classic()+ylab("Total Basal Area per Acre (ft^2/acre)")

plot=plot +annotate("text", x="2003", y=50,size=3, label="Fire", color="red")
plot

ggsave("C:/Users/edeegan/OneDrive - DOI/FFIanalysis/Analysis/PSME_Plots/tree_basal_area_NSRF.png", width=7, height=3)

#sum basal area by year and plot
tree_basal=tree_1 %>% group_by(Year, MacroPlot.Name, SubFrac) %>% summarize(total_ba=sum(basal_area))
#adding acres
tree_basal=tree_basal %>%
  mutate(acres = 0.24710538*SubFrac) %>% mutate(density=total_ba/acres)
#sum density by plot and year
tree_basal=tree_basal %>% group_by(Year, MacroPlot.Name) %>% summarize(totalba_peracre=sum(density))

#averaging/plot by year
tree_basal_summary=tree_basal %>% group_by(Year) %>% summarize(avg_plot_ba=mean(totalba_peracre))




ggplot(tree_basal, aes(x=totalba_peracre, fill=MacroPlot.Name))+geom_histogram()+facet_grid(rows=vars(Year))+theme_classic()
ggsave("C:/Users/edeegan/OneDrive - DOI/FFIanalysis/Analysis/PSME_Plots/basalarea_byplot.png", width=7, height=4)

library(ggridges)
library(viridis)
ggplot(tree_basal, aes(x=totalba_peracre, y=Year, fill=after_stat(x)))+geom_density_ridges_gradient()+
  scale_colour_gradient(low = "#132B43",
                        high = "#56B1F7")+theme_classic()+
  scale_y_discrete(limits=rev)
ggsave("C:/Users/edeegan/OneDrive - DOI/FFIanalysis/Analysis/PSME_Plots/basalarea_distribution.png", width=7, height=4)



basal_a_model=aov(totalba_peracre ~ as.factor(Year), data=tree_basal)
anova(basal_a_model)
tukey=TukeyHSD(basal_a_model, conf.level=.95)


# I need to group the treatments that are not different each other together.
generate_label_df <- function(TUKEY, variable){
  
  # Extract labels and factor levels from Tukey post-hoc 
  Tukey.levels <- TUKEY[[variable]][,4]
  Tukey.labels <- data.frame(multcompLetters(Tukey.levels)['Letters'])
  
  #I need to put the labels in the same order as in the boxplot :
  Tukey.labels$treatment=rownames(Tukey.labels)
  Tukey.labels=Tukey.labels[order(Tukey.labels$treatment) , ]
  return(Tukey.labels)
}

labels=generate_label_df(tukey, "as.factor(Year)")



#plotting basal area against canopy cover data
#need average canopy cover per plot

canopy=canopy %>% group_by(Year, MacroPlot.Name) %>% summarize(cover=mean(Cover))

canopy_basal=inner_join(canopy, tree_basal, by = join_by(Year, MacroPlot.Name))

ggplot(canopy_basal, aes(x=cover, y=totalba_peracre))+
  geom_point()+geom_smooth(method="lm")+theme_classic()

ggsave("C:/Users/edeegan/OneDrive - DOI/FFIanalysis/Analysis/PSME_Plots/basalarea_v_cover.png", width=5, height=4)

model <- lm(totalba_peracre~cover, data=canopy_basal)
summary(model)



