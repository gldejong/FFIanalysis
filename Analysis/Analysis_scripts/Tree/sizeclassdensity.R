#clearing environment - fresh start!
rm(list = ls())

library(tidyverse)
library(hrbrthemes)
library(reshape2)
library(ggpubr)

plot_order=c("PSME-01" ,"PSME-02", "PSME-03", "PSME-04", "PSME-05" ,"PSME-06" , "PSME-07", "PSME-08" ,"PSME-09", "PSME-10")
#set working directory - CHANGE TO LOCAL SPECIFIC PATH
setwd("C:/Users/edeegan/OneDrive - DOI/FFIanalysis/Analysis/")
#load in tree data - CHANGE TO LOCAL SPECIFIC PATH
tree=read.csv("C:/Users/edeegan/OneDrive - DOI/Fire_project/Fire_project/SAGU_data/PSME/PSME_Trees - Individuals (metric)_XPT.csv")
#formatting date column
tree$Date_format=as.Date(tree$Date, format="%m/%d/%Y")
#creating new column for just year
tree$Year=str_split_i(tree$Date_format, "-", 1)

#filtering for 2001, 2003, 2023
tree=tree %>% filter(Year=="2001" | Year=="2003" | Year=="2023")

#filtering for live trees
tree=tree %>% filter(Status=="L")


poles=tree[which(tree$DBH<=15.1),]
medium=tree[which(tree$DBH>15.1 & tree$DBH<30),]
overstory=tree[which(tree$DBH>=30),]

poles_density=poles %>% group_by(Year, MacroPlot.Name) %>% count() %>% 
  mutate(density=n/(0.24710538*4))
poles_density_2001=poles_density %>% filter(Year=="2001")
poles_density_2003=poles_density %>% filter(Year=="2003")

#adding blank years
a=c("2003", "PSME-03", as.integer(0), as.numeric(0))
b=c("2003", "PSME-07",as.integer(0), as.numeric(0))
c=c("2003", "PSME-08", as.integer(0), as.numeric(0))
d=c("2003", "PSME-09", as.integer(0), as.numeric(0))
df=as.data.frame(rbind(a, b, c, d))
colnames(df)=c("Year", "MacroPlot.Name", "n", "density")
df$n=as.integer(df$n)
df$density=as.numeric(df$density)
poles_density_2003=poles_density_2003=rbind(poles_density_2003,df )
poles_density_2003$MacroPlot.Name <- factor(poles_density_2003$MacroPlot.Name, levels = plot_order)
poles_density_2003 <- poles_density_2003 %>% arrange(MacroPlot.Name)
#adding blank years done


poles_density_2023=poles_density %>% filter(Year=="2023")
a=c("2023", "PSME-09", as.integer(0), as.numeric(0))
df=as.data.frame(t(a))
colnames(df)=c("Year", "MacroPlot.Name", "n", "density")
df$n=as.integer(df$n)
df$density=as.numeric(df$density)
poles_density_2023=poles_density_2023=rbind(poles_density_2023,df )
poles_density_2023$MacroPlot.Name <- factor(poles_density_2023$MacroPlot.Name, levels = plot_order)
poles_density_2023 <- poles_density_2023 %>% arrange(MacroPlot.Name)

##POLES ARE GOOD my goodness


medium_density=medium %>% group_by(Year, MacroPlot.Name) %>% count() %>% 
  mutate(density=n/(0.24710538*4)) 
medium_density_2001=medium_density %>% filter(Year=="2001")
medium_density_2003=medium_density %>% filter(Year=="2003")

a=c("2003", "PSME-03", as.integer(0), as.numeric(0))
c=c("2003", "PSME-08", as.integer(0), as.numeric(0))
d=c("2003", "PSME-09", as.integer(0), as.numeric(0))
df=as.data.frame(rbind(a, c, d))
colnames(df)=c("Year", "MacroPlot.Name", "n", "density")
df$n=as.integer(df$n)
df$density=as.numeric(df$density)
medium_density_2003=medium_density_2003=rbind(medium_density_2003,df )
medium_density_2003$MacroPlot.Name <- factor(medium_density_2003$MacroPlot.Name, levels = plot_order)
medium_density_2003 <- medium_density_2003 %>% arrange(MacroPlot.Name)



medium_density_2023=medium_density %>% filter(Year=="2023")

a=c("2023", "PSME-07", as.integer(0), as.numeric(0))
c=c("2023", "PSME-08", as.integer(0), as.numeric(0))
df=as.data.frame(rbind(a, c))
colnames(df)=c("Year", "MacroPlot.Name", "n", "density")
df$n=as.integer(df$n)
df$density=as.numeric(df$density)
medium_density_2023=medium_density_2023=rbind(medium_density_2023,df )
medium_density_2023$MacroPlot.Name <- factor(medium_density_2023$MacroPlot.Name, levels = plot_order)
medium_density_2023 <- medium_density_2023 %>% arrange(MacroPlot.Name)

overstory_density=overstory %>% group_by(Year, MacroPlot.Name) %>% count() %>% 
  mutate(density=n/(0.24710538*4)) 
overstory_density_2001=overstory_density %>% filter(Year=="2001")
overstory_density_2003=overstory_density %>% filter(Year=="2003")

a=c("2003", "PSME-03", as.integer(0), as.numeric(0))
c=c("2003", "PSME-08", as.integer(0), as.numeric(0))
d=c("2003", "PSME-09", as.integer(0), as.numeric(0))
df=as.data.frame(rbind(a, c, d))
colnames(df)=c("Year", "MacroPlot.Name", "n", "density")
df$n=as.integer(df$n)
df$density=as.numeric(df$density)
overstory_density_2003=overstory_density_2003=rbind(overstory_density_2003,df )
overstory_density_2003$MacroPlot.Name <- factor(overstory_density_2003$MacroPlot.Name, levels = plot_order)
overstory_density_2003 <- overstory_density_2003 %>% arrange(MacroPlot.Name)


overstory_density_2023=overstory_density %>% filter(Year=="2023")

a=c("2023", "PSME-03", as.integer(0), as.numeric(0))
c=c("2023", "PSME-08", as.integer(0), as.numeric(0))
d=c("2023", "PSME-09", as.integer(0), as.numeric(0))
df=as.data.frame(rbind(a, c, d))
colnames(df)=c("Year", "MacroPlot.Name", "n", "density")
df$n=as.integer(df$n)
df$density=as.numeric(df$density)
overstory_density_2023=overstory_density_2023=rbind(overstory_density_2023,df )
overstory_density_2023$MacroPlot.Name <- factor(overstory_density_2023$MacroPlot.Name, levels = plot_order)
overstory_density_2023 <- overstory_density_2023 %>% arrange(MacroPlot.Name)



jpeg("PSME_Plots/sizeclassdensity.jpg", height=2000, width=3000, units="px")
# Create a bar plot of mean CrScPct values by MacroPlot.Name
par(mfrow=c(3,3))
barplot(poles_density_2001$density, names.arg = poles_density_2001$MacroPlot.Name, col = "#deebf7", cex.axis = 3, ylim = c(0,36))
barplot(medium_density_2001$density, names.arg = medium_density_2001$MacroPlot.Name, col = "#e5f5e0", cex.axis = 3, ylim = c(0,36))
barplot(overstory_density_2001$density, names.arg = overstory_density_2001$MacroPlot.Name, col = "#fee0d2", cex.axis = 3, ylim = c(0,36))

barplot(poles_density_2003$density, names.arg = poles_density_2003$MacroPlot.Name, col = "#9ecae1", cex.axis = 3, ylim = c(0,36))
barplot(medium_density_2003$density, names.arg = medium_density_2003$MacroPlot.Name, col = "#a1d99b", cex.axis = 3, ylim = c(0,36))
barplot(overstory_density_2003$density, names.arg = overstory_density_2003$MacroPlot.Name, col = "#fc9272", cex.axis = 3, ylim = c(0,36))

barplot(poles_density_2023$density, names.arg = poles_density_2023$MacroPlot.Name, col = "#3182bd", cex.axis = 3, ylim = c(0,36))
barplot(medium_density_2023$density, names.arg = medium_density_2023$MacroPlot.Name, col = "#31a354", cex.axis = 3, ylim = c(0,36))
barplot(overstory_density_2023$density, names.arg = overstory_density_2023$MacroPlot.Name, col = "#de2d26", cex.axis = 3, ylim = c(0,36))

dev.off()

