setwd("C:/Users/edeegan/OneDrive - DOI/Fire_project/Fire_project/")

library(tidyverse)
library(ggrepel)
library(multcompView)
#clearing environment - fresh start!
rm(list = ls())

cover=read.csv("SAGU_data/PSME/PSME_Cover - Species Composition (metric)_XPT.csv", na.strings=c("","NA"))
#formatting date column
cover$Date=as.Date(cover$Date, format="%m/%d/%Y")
#creating new column for just year
cover$Year=str_split_i(cover$Date, "-", 1)

tree=read.csv("C:/Users/edeegan/OneDrive - DOI/Fire_project/Fire_project/SAGU_data/PSME/PSME_Trees - Individuals (metric)_XPT.csv")
#formatting date column
tree$Date=as.Date(tree$Date, format="%m/%d/%Y")
#creating new column for just year
tree$Year=str_split_i(tree$Date, "-", 1)

#combining 1990, 1991, 1992
tree[which(tree$Year=='1990' | tree$Year=='1991'), "Year"]='1992'


#TREE BASAL AREA CALCULATION

tree$basal_area=(tree$DBH^2)*0.005454
tree$basal_area=as.numeric(tree$basal_area)
tree_1=tree[-which(is.na(tree$basal_area)),]
numberofplots=tree_1 %>% group_by(Year, MacroPlot.Name) %>% count(MacroPlot.Name)
numberofplots$n=0.24710538 #acreage for one plot
numberofplots$n=as.numeric(numberofplots$n)
numberofplots=numberofplots %>% group_by(Year) %>% summarize(acres=sum(n))


tree_1=tree_1[-which(tree_1$Status=="D"),]
#tree_1=tree_1[which(tree_1$Year=="2008" | tree_1$Year=="2004" | tree_1$Year=="2013" | tree_1$Year=="2023"),]

tree_basal=tree_1 %>% group_by(Year, Species.Symbol) %>% summarize(total_ba=sum(basal_area))

tree_basal=tree_basal %>%
  mutate(acres = recode(Year, 
                    '1992' = as.numeric(numberofplots[which(numberofplots$Year=='1992'),"acres"]), 
                    '1997' = as.numeric(numberofplots[which(numberofplots$Year=='1997'),"acres"]),
                    '2001' = as.numeric(numberofplots[which(numberofplots$Year=='2001'),"acres"]),
                    '2003' = as.numeric(numberofplots[which(numberofplots$Year=='2003'),"acres"]),
                    '2004' = as.numeric(numberofplots[which(numberofplots$Year=='2004'),"acres"]),
                    '2008' = as.numeric(numberofplots[which(numberofplots$Year=='2008'),"acres"]),
                    '2013' = as.numeric(numberofplots[which(numberofplots$Year=='2013'),"acres"]),
                    '2023' = as.numeric(numberofplots[which(numberofplots$Year=='2023'),"acres"]),))



tree_basal$total_basal_area_per_acre=tree_basal$total_ba/tree_basal$acres
#renaming species code to common name - CHANGE FOR LOCAL SPECIES
tree_basal=tree_basal %>%
  mutate(Species.Symbol = recode(Species.Symbol, 
                                 'ABCO1' = 'White Fir', 
                                 'QUGA1' = 'Gambels Oak',
                                 'PIPO1' = 'Ponderosa Pine',
                                 'PIST1' = 'Southwestern White Pine',
                                 'PSME1' = 'Douglas Fir'))

plot=ggplot(tree_basal, aes(x=Year, y=total_basal_area_per_acre, fill=Species.Symbol))+
  geom_bar(position="stack", stat="identity")+theme_classic()+ylab("Total Basal Area per Acre (ft^2/acre)")

plot=plot +annotate("text", x="2003", y=50,size=3, label="Fire", color="red")
plot=plot+ annotate("text", x="2008", y=700,size=2, label="*only 3 plots")+
 annotate("text", x="2008", y=660,size=2, label="dbh measured")
plot

ggsave("C:/Users/edeegan/OneDrive - DOI/FFIanalysis/Analysis/PSME_Plots/tree_basal_area.png", width=7, height=3)

#ggarrange(plot, canopy_graph_2)

basal_a_model=aov(total_basal_area_per_acre ~ as.factor(Year) + Species.Symbol, data=tree_basal)
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
labels$Year=labels$treatment

#tree_basal=tree_basal %>%
 # mutate(Letters = recode(Year, 
 #                         '1992' = labels[which(labels$Year=='1992'),"Letters"], 
 #                         '1997' = labels[which(labels$Year=='1997'),"Letters"],
 #                         '2001' = labels[which(labels$Year=='2001'),"Letters"],
 #                         '2003' = labels[which(labels$Year=='2003'),"Letters"],
 #                         '2004' = labels[which(labels$Year=='2004'),"Letters"],
 #                         '2008' = labels[which(labels$Year=='2008'),"Letters"],
  #                        '2013' = labels[which(labels$Year=='2013'),"Letters"],
   #                       '2023' = labels[which(labels$Year=='2023'),"Letters"]))



#plot+geom_bar(stat="identity")+aes(x=0, y=tree_basal$Letters, label=tree_basal$Letters)+
 # theme()+
  #geom_text(position="stack")
#trying to add tukey labels so close! update going to do it manually womp womp

