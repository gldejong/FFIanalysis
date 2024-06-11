###Sample event analysis
library(tidyverse)
#clearing environment - fresh start!
rm(list = ls())
#reading in fuel data
samps <- read.csv("C:/Users/edeegan/OneDrive - DOI/Fire_project/Fire_project/SAGU_data/PSME/SAGU_SampleEventReport.csv",
                  sep=",")

#formatting date column
samps$Date_format=as.Date(samps$SampleEvent_Date, format="%m/%d/%Y")
#creating new column for just year
samps$Year=str_split_i(samps$Date_format, "-", 1)



summary = samps %>% count(ProjectUnit_Name, MacroPlot_Name)

summary = summary %>% count(ProjectUnit_Name)

colnames(summary)=c("Project Unit", "Number of Plots")
