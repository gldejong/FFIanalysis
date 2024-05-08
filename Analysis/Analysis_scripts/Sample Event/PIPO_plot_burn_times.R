#clearing environment - fresh start!
setwd("C:/Users/edeegan/OneDrive - DOI/FFIanalysis/Analysis/")
rm(list = ls())
library(gganimate)
library(tidyverse)
library(gifski)
###Visualization of when PIPO plots burned

plots=c(1:6, 8:22, 24:30)
plots=paste("PIPO-", plots, sep="")

burn1=c(1993,
1993,
1996,
2003,
2003,
1993,
1993,
1996,
1993,
1997,
1994,
1994,
1996,
1994,
1996,
1996,
1997,
1997,
1997,
2009,
1996,
1996,
1996,
1996,
1996,
1998,
1998,
1998)

burn2=c(2002,
2002,
2002,
NA,
NA,
2002,
2002,
2002,
2002,
2004,
1996,
1996,
2002,
1996,
2002,
2002,
2004,
2004,
2004,
NA,
2002,
2002,
2002,
2002,
2002,
NA,
NA,
NA)


burn3=c(2020,
2020,
2019,
NA,
NA,
2020,
2020,
2019,
2020,
2014,
2002,
2002,
2019,
2002,
2010,
2019,
2020,
2014,
2020,
NA,
2019,
2019,
NA,
NA,
2019,
NA,
NA,
NA)

burn4=c(rep(NA, 9), 2020,
        2010,
        2010,
        NA,
        2010,
        2019,
        NA,
        NA,
        2020, rep(NA, 10))

burn5=c(rep(NA, 10), 2019,
        2019,
        NA,
        2019, rep(NA, 14)
)

burnhistory=cbind(plots, burn1, burn2, burn3, burn4, burn5)
burnhistory=as.data.frame(burnhistory)
rownames(burnhistory)=plots

burn_history_long=burnhistory %>% pivot_longer(cols=starts_with("burn"),
                             names_to="burn",
                             names_prefix = "burn",
                             values_to = "year",
                             values_drop_na = TRUE)


burn_history_long$year=paste(burn_history_long$year, "-01-01", sep="")
burn_history_long$year=as_date(burn_history_long$year)
burn_history_long$fire="yes"

p=ggplot(burn_history_long, aes(x=year,fill=burn))+geom_bar(width=2)+facet_wrap(~factor(plots,levels=c("PIPO-1",
"PIPO-2","PIPO-3","PIPO-4","PIPO-5","PIPO-6", "PIPO-8","PIPO-9","PIPO-10","PIPO-11","PIPO-12","PIPO-13",
"PIPO-14","PIPO-15","PIPO-16","PIPO-17","PIPO-18","PIPO-19","PIPO-20" ,"PIPO-21", "PIPO-22", "PIPO-24", "PIPO-25", "PIPO-26", "PIPO-27", "PIPO-28",
"PIPO-29", "PIPO-30")))+
  theme(axis.text.y=element_blank(), axis.title.y=element_blank(), axis.ticks=element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=rel(1)),
        panel.background = element_blank())+scale_x_discrete(breaks=burn_history_long$year)+
  scale_fill_brewer(palette = "Reds")#+
 #transition_states(year)+
 # shadow_mark()
#p=animate(p, renderer = gifski_renderer(loop = FALSE), width=800, height=800)

#anim_save(animation=p, filename="PIPO_burn_history.gif")

ggsave("PIPO_burn_history.png", width=10, height=10)
