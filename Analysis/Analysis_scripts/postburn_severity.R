###Pbsev analysis
#clearing environment - fresh start!
rm(list = ls())

library(tidyverse)
library(hrbrthemes)
library(reshape2)
library(ggpubr)

#set working directory - CHANGE TO LOCAL SPECIFIC PATH
setwd("C:/Users/edeegan/OneDrive - DOI/FFIanalysis/Analysis/")
#load in tree data - CHANGE TO LOCAL SPECIFIC PATH
tree=read.csv("C:/Users/edeegan/OneDrive - DOI/Fire_project/Fire_project/SAGU_data/PSME/PSME_Trees - Individuals (metric)_XPT.csv")
#formatting date column
tree$Date_format=as.Date(tree$Date, format="%m/%d/%Y")
#creating new column for just year
tree$Year=str_split_i(tree$Date_format, "-", 1)

#filtering for 2003
tree_2003=tree %>% filter(Year=="2003")

#CharHt, ScorchHt, CrScPct

tree_long <- gather(tree_2003, condition, measurement, CharHt:ScorchHt)

ggplot(tree_long, aes(x=MacroPlot.Name, y=measurement, fill=condition))+
  geom_boxplot()+geom_jitter(size=0.4, alpha=0.9)+theme_ipsum()



tree_2003=tree_2003[complete.cases(tree_2003[ , c("CharHt", "ScorchHt", "CrScPct")]),]

ggplot(tree_2003, aes(x=MacroPlot.Name, y=CrScPct))+
  geom_boxplot()+geom_jitter(size=0.4, alpha=0.9)

fuels=read.csv("C:/Users/edeegan/OneDrive - DOI/Fire_project/Fire_project/SAGU_data/PSME/PSME_SAGU_Report_SurfaceFuels_E.csv")
#formating fuels data frame as tibble
fuels<-as_tibble(fuels)
fuels=rename(fuels, MacroPlot.Name=`Macroplot`)
fuels$'1000hr'=fuels$X.3_Snd+fuels$X.3_Rot
fuels <- fuels %>% select(!starts_with("X"))
fuels <- fuels %>% mutate(year=NA)


#LUMP 1990-92 together and make a note because they weren't full samples

fuels <- fuels %>% mutate(year=case_when(MonStatusOrd==0 ~ 1992, 
                                         MonStatusOrd==1 ~ 1997,
                                         MonStatusOrd==2 ~ 2001,
                                         MonStatusOrd==3 ~ 2003,
                                         MonStatusOrd==4 ~ 2004,
                                         MonStatusOrd==5 ~ 2008,
                                         MonStatusOrd==6 ~ 2013,
                                         MonStatusOrd==7 ~ 2023))
fuels=fuels %>% filter(year=="2003")

ggplot(fuels, aes(x=MacroPlot.Name, y=TotalAll))+geom_col()
ggplot(tree, aes(x=MacroPlot.Name, y=CrScPct))+geom_histogram(stat="identity")

tree$MacroPlot.Name <- factor(tree$MacroPlot.Name, 
                              levels = c("PSME-01", "PSME-02", "PSME-03", "PSME-04", "PSME-05" ,"PSME-06", "PSME-07", "PSME-08", "PSME-09", "PSME-10"))
# Calculate mean CrScPct for each MacroPlot.Name
mean_crsc_pct <- aggregate(CrScPct ~ MacroPlot.Name, data = tree, FUN = mean)
# Calculate mean CharHt for each MacroPlot.Name
mean_charht <- aggregate(CharHt ~ MacroPlot.Name, data = tree, FUN = mean)
# Calculate mean ScorchHt for each MacroPlot.Name
mean_scorchht <- aggregate(ScorchHt ~ MacroPlot.Name, data = tree, FUN = mean)

jpeg("PSME_Plots/pbsev.jpg", height=1000, width=1000, units="px")
# Create a bar plot of mean CrScPct values by MacroPlot.Name
par(mfrow=c(4,1))


barplot(fuels$TotalAll, names.arg=fuels$MacroPlot.Name, col="blue", main = "Total Fuel Loading Pre-Fire by MacroPlot.Name", xlab = "MacroPlot.Name", ylab = "Total Fuel Loading (count/acre)")

barplot(mean_crsc_pct$CrScPct, names.arg = mean_crsc_pct$MacroPlot.Name, col = "red", main = "Mean Crown Scorch % values by MacroPlot.Name", xlab = "MacroPlot.Name", ylab = "Mean CrScPct value")

barplot(mean_charht$CharHt, names.arg = mean_charht$MacroPlot.Name, col = "green", main = "Mean Char Height values by MacroPlot.Name", xlab = "MacroPlot.Name", ylab = "Mean Char value")


barplot(mean_scorchht$ScorchHt, names.arg = mean_scorchht$MacroPlot.Name, col = "purple", main = "Mean Scorch Height values by MacroPlot.Name", xlab = "MacroPlot.Name", ylab = "Mean Scorch value")


dev.off()



poles=tree[which(tree$DBH<=15.1 & tree$Year=="2001"),]

poles_density=poles %>% group_by(MacroPlot.Name) %>% count() %>% 
  mutate(density=n/(0.24710538*4)) 


poles_height=poles[complete.cases(poles[ , "Ht"]),]


poles_height=poles_height %>% group_by(MacroPlot.Name) %>% 
  summarize(avg_height=mean(Ht))
jpeg("PSME_Plots/pbsev_v_poles.jpg", height=1000, width=1000, units="px")
# Create a bar plot of mean CrScPct values by MacroPlot.Name
par(mfrow=c(6,1))

barplot(fuels$TotalAll, names.arg=fuels$MacroPlot.Name, col="#d73027", main = "Total Fuel Loading Pre-Fire by MacroPlot.Name", xlab = "MacroPlot.Name", ylab = "Total Fuel Loading (count/acre)")


barplot(poles_height$avg_height, names.arg = poles_height$MacroPlot.Name, col = "#fc8d59", main = "Pre-Fire Mean Pole Tree Heights by MacroPlot.Name", xlab = "MacroPlot.Name", ylab = "Height (m)")


barplot(poles_density$density, names.arg = poles_density$MacroPlot.Name, col = "#fee090", main = "Pre-Fire Mean Pole Tree Density by MacroPlot.Name", xlab = "MacroPlot.Name", ylab = "Density (count/acre)")

barplot(mean_crsc_pct$CrScPct, names.arg = mean_crsc_pct$MacroPlot.Name, col = "#e0f3f8", main = "Mean Crown Scorch % values by MacroPlot.Name", xlab = "MacroPlot.Name", ylab = "Mean CrScPct value")

barplot(mean_charht$CharHt, names.arg = mean_charht$MacroPlot.Name, col = "#91bfdb", main = "Mean Char Height values by MacroPlot.Name", xlab = "MacroPlot.Name", ylab = "Mean Char value")



barplot(mean_scorchht$ScorchHt, names.arg = mean_scorchht$MacroPlot.Name, col = "#4575b4", main = "Mean Scorch Height values by MacroPlot.Name", xlab = "MacroPlot.Name", ylab = "Mean Scorch value")
dev.off()

#d73027
#fc8d59
#fee090
#e0f3f8
#91bfdb
#4575b4