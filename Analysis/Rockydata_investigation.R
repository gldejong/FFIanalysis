setwd("C:/Users/edeegan/OneDrive - DOI/FFIanalysis/Analysis/")

seeds=read.csv("ROMO_Seedlings-Density - Quadrats (metric)_PIPO 10.csv")
trees=read.csv("ROMO_Trees - Individuals (metric)_PIPO 22.csv")

#formatting date column
trees$Date=as.Date(trees$Date, format="%m/%d/%Y")
#creating new column for just year
trees$Year=str_split_i(trees$Date, "-", 1)

#formatting date column
seeds$Date=as.Date(seeds$Date, format="%m/%d/%Y")
#creating new column for just year
seeds$Year=str_split_i(seeds$Date, "-", 1)


ggplot(seeds)+geom_bar(aes(x=Height, y=Count), stat="identity")+facet_grid(rows=vars(Year))+theme_classic()

#average seedling height calculation
seeds$avg=seeds$Count*seeds$Height
seeds=seeds[-which(is.na(seeds$avg)),]

seeds %>% group_by(Year) %>% summarize(new=mean(avg))


seeds_1=seeds[, c("Year", "MacroPlot.Name", "Count", "Height")]
h=c()
y=c()
p=c()
for(x in 1:nrow(seeds)){
  h=c(h, rep(seeds[x,"Height"],seeds[x,"Count"]))
  y=c(y, rep(seeds[x,"Year"],seeds[x,"Count"]))
  p=c(p, rep(seeds[x,"MacroPlot.Name"],seeds[x,"Count"]))
}
seeds_2=cbind(h, y, p)
seeds_2=as.data.frame(seeds_2)
seeds_2$h=as.numeric(seeds_2$h)

seeds_2=seeds_2 %>% group_by(y) %>% summarize(avg_height=mean(h))
seeds_2$avg_height=as.numeric(seeds_2$avg_height)
percent_increase=c(0)
for(i in 2:nrow(seeds_2)){
  percent_increase=c(percent_increase, ((seeds_2[i, "avg_height"]-seeds_2[(i-1), "avg_height"])/seeds_2[(i-1), "avg_height"])*100)
}
percent_increase=as.vector(percent_increase)
percent_increase=as.numeric(percent_increase)
percent_increase=round(percent_increase, 0)
seeds_2$percent_change=paste(percent_increase, "%")
seeds_2$avg_height=round(seeds_2$avg_height, 1)
