##Fuel analysis for AFE

# Add packages here
library(knitr)
library(tidyverse)
library(ggplot2)
library(car)
library(dplyr)
library(car)
library(broom)
library(emmeans)
library(lmerTest)
library(lme4)
library(pbkrtest)
library(gganimate)
library(ggstream)

rm(list = ls())

fuels <- read.csv("C:/Users/edeegan/OneDrive - DOI/Fire_project/Fire_project/PSME_data/PSME_SAGU_Report_SurfaceFuels_E.csv",
                  sep=",")


fuels_t<-as_tibble(fuels)
fuels_t=rename(fuels_t, plot=`Macroplot`)

fuels <- fuels_t %>% select(!starts_with("X"))
fuels <- fuels %>% mutate(year=NA)
fuels$'1000hr'=fuels_t$X.3_Snd+fuels_t$X.3_Rot

#LUMP 1990-92 together and make a note because they weren't full samples

fuels <- fuels %>% mutate(year=case_when(MonStatusOrd==0 ~ 1992, 
                                         MonStatusOrd==1 ~ 1997,
                                         MonStatusOrd==2 ~ 2001,
                                         MonStatusOrd==3 ~ 2003,
                                         MonStatusOrd==4 ~ 2004,
                                         MonStatusOrd==5 ~ 2008,
                                         MonStatusOrd==6 ~ 2013,
                                         MonStatusOrd==7 ~ 2023))
#weird data here so cutting it for conference, come back to later and check if its the same people recording fuels each time
#because plots 1 and 7 have some crazy high numbers
fuels <- fuels %>% filter(year!='1992' & year!='1997')


fuels_factor1 <- fuels %>%
  mutate(plot=factor(plot))

fuels_factor2 <- fuels_factor1 %>%
  mutate(year=factor(year))


fuels_factor2_summ <- fuels_factor2 %>%
  group_by(`Year`=year) %>%
  reframe(n=n(),
            `Min.`=c(min(OneHr),min(TenHr), min(HunHr)),
            `Max.`=c(max(OneHr),max(TenHr),max(HunHr)),
            `Mean`=c(mean(OneHr),mean(TenHr),mean(HunHr)),
            `SD`=c(sd(OneHr),sd(TenHr),sd(HunHr)),
            `SE`=c(sd(OneHr)/sqrt(n),sd(TenHr)/sqrt(n),sd(HunHr)/sqrt(n)),
            'Size_class'=c("One", "Ten", "Hun"))

kable(fuels_factor2_summ, booktabs=T, digits=3)


fuels_factor_total_duff_litt <- fuels_factor2 %>%
  group_by(`Year`=year) %>%
  reframe(n=n(),
          `Min.`=c(min(TotalWood),min(Duff), min(Litter)),
          `Max.`=c(max(TotalWood),max(Duff),max(Litter)),
          `Mean`=c(mean(TotalWood),mean(Duff),mean(Litter)),
          `SD`=c(sd(TotalWood),sd(Duff),sd(Litter)),
          `SE`=c(sd(TotalWood)/sqrt(n),sd(Duff)/sqrt(n),sd(Litter)/sqrt(n)),
          'Size_class'=c("Total", "Duff", "Litter"))

kable(fuels_factor_total_duff_litt, booktabs=T, digits=3)



fuels_prevpost <- fuels_factor2 %>% filter(year!='2003' & year!='2004'& year!='2013')

fuels_prevpost=fuels_prevpost %>%
pivot_longer(cols=c(OneHr, TenHr, HunHr, '1000hr'), names_to="Size_class", values_to="Fuel_Loading")

#2001 - 2023 <.0001 ones
#0.7518 tens
#0.0228 huns
#0.0510 thou

ggplot() +
  theme_bw() +
  geom_boxplot(aes(y=Fuel_Loading, x=year, color=Size_class),
               data=fuels_prevpost) +
  geom_jitter(aes(y=Fuel_Loading, x=year, color=Size_class),
              height=0,
              data=fuels_prevpost, size=1) +
  ylab("Fuel loading (tons/acre)") +
  ggtitle("Fuel loading (tons/acre)")+facet_wrap(~Size_class)

ggsave('boxplot_2.png')

#look in contrast notes to add letters 

#ask windy how she would calculate total fuel load

#add a grey band of 30-50% range reducing from the total 2001 number
#compare now to pre fire levels with percentage
#run linear model on total as well - think about what makes sense
#math to calculate if 30-50% reduction in total fuels objective was reached

totalprefire <- fuels_factor2 %>% filter(year==2001) %>% summarise(mean=mean(TotalWood))
totalprefire=as.numeric(totalprefire)
range_min <- totalprefire*0.5
range_max <- totalprefire*0.7
totalpostfire <- fuels_factor2 %>% filter(year==2004) %>% summarise(mean=mean(TotalWood))
totalpostfire=as.numeric(totalpostfire)
total2023 <- fuels_factor2 %>% filter(year==2023) %>% summarise(mean=mean(TotalWood))
total2023=as.numeric(total2023)

#Eva's timeline graph
fuels_factor2_summ %>%
  ggplot(aes(x=Year, y=Mean, group=Size_class, color=Size_class))+
  geom_line()+geom_point(size=5)+theme_classic()+ylab("Fuel loading (tons/acre)")

#timeline graph 2
fuels_factor2 %>%
  ggplot(aes(x=year, y=TotalWood, group=plot, color=plot))+
  geom_line()+geom_point(size=5)+theme_classic()+ylab("Total Fuel Loading (tons/acre)")+geom_vline(xintercept='2003', color='red', linewidth=1.5)


fuels_factor2$year=as.Date(fuels_factor2$year, "%Y")

fuels_factor2 %>% ggplot(aes(x=year, y=TotalWood, group_by(plot)))+geom_bar(stat="summary", fun="mean")+
  stat_summary(fun.data = mean_se, geom = "errorbar", size=0.1, alpha=0.5)+
  geom_vline(xintercept=as.Date("2003-11-20", format="%Y-%m-%d"), color='red', linewidth=1, alpha=0.3)+theme_classic()+
  geom_hline(yintercept=range_min)+geom_hline(yintercept=range_max)+
  annotate('rect', xmin=as.Date("2001-11-20", format="%Y-%m-%d"), xmax=as.Date("2023-11-20", format="%Y-%m-%d"), ymin=range_min, ymax=range_max, alpha=.2, fill='darkgrey')+
  annotate("text", x=as.Date("2002-06-20", format="%Y-%m-%d"), y=totalprefire+5.5, label="Avg Pre-Fire = ", size=3)+
  annotate("text", x=as.Date("2002-06-20", format="%Y-%m-%d"), y=totalprefire+4.3, label="18.5 (tons/acre)", size=3)+
  annotate("text", x=as.Date("2004-11-20", format="%Y-%m-%d"), y=range_min+1, label="50% decrease", size=3)+
  annotate("text", x=as.Date("2008-06-20", format="%Y-%m-%d"), y=range_min+1, label="= 9.3 (tons/acre)", size=3)+
  annotate("text", x=as.Date("2004-11-20", format="%Y-%m-%d"), y=range_max+1, label="30% decrease", size=3)+
  annotate("text", x=as.Date("2008-06-20", format="%Y-%m-%d"), y=range_max+1, label="= 13.0 (tons/acre)", size=3)+ 
  annotate("text", x=as.Date("2007-11-20", format="%Y-%m-%d"), y=totalpostfire+0.2, label="Avg Post-Fire = ", size=3)+
  annotate("text", x=as.Date("2007-11-20", format="%Y-%m-%d"), y=totalpostfire-1, label="7.9 (tons/acre)", size=3)+
  annotate("text", x=as.Date("2021-11-20", format="%Y-%m-%d"), y=total2023+2, label="Avg 2023 = ", size=3)+
  annotate("text", x=as.Date("2021-11-20", format="%Y-%m-%d"), y=total2023+1, label="30 (tons/acre)", size=3)+
  annotate("text", x=as.Date("2020-11-20", format="%Y-%m-%d"), y=total2023-2, label="162%", size=5, color='red')+
  annotate("text", x=as.Date("2020-11-20", format="%Y-%m-%d"), y=total2023-3.5, label="increase", size=5, color='red')+
  annotate("text", x=as.Date("2020-11-20", format="%Y-%m-%d"), y=total2023-5, label="(from 2001)", size=3, color='red')+
  ylab("Average Fuel Loading (tons/acre)")+xlab("Year")

ggsave('barplot.png')







fuels_factor2_long=fuels_factor2 %>%
  pivot_longer(cols=c(OneHr, TenHr, HunHr), names_to="Size_class", values_to="Fuel_Loading")

#Lauras graph - don't know if its useful
ggplot() +
  geom_histogram(aes(x=Fuel_Loading, fill=Size_class),
                 bins=2,
                 data=fuels_factor2_long) +
  xlab("Average fuels Fuel_Loading ") +
  ggtitle("Average fuels Fuel_Loading ")



#linear models
#poisson uses count data, doesn't like non integers. log normal data usually rates
#gaussian is the same as a normal distribution so lmer model works
#repeated measures anova, nothing crazy
#because we have permenant plots but multiple levels of fuel loading
#later we can look at covariates but started here
fuels_factor2$year=as.factor(fuels_factor2$year)

fuels_lmer_One <- lmer(OneHr~year+ (1|plot),
                       data=fuels_factor2)
summary(fuels_lmer_One)
#fixed effects compares everything to 1990

fuels_lmer_Ten <- lmer(TenHr~year + (1|plot),
                       data=fuels_factor2)
summary(fuels_lmer_Ten)

fuels_lmer_Hun <- lmer(HunHr~year + (1|plot),
                       data=fuels_factor2)
summary(fuels_lmer_Hun)

fuels_factor2=rename(fuels_factor2, thouhr='1000hr')

fuels_lmer_1000 <- lmer(thouhr~year + (1|plot),
                       data=fuels_factor2)
summary(fuels_lmer_1000)


#anovas
anova(fuels_lmer_One) #year is having an effect

anova(fuels_lmer_Ten)

anova(fuels_lmer_Hun)

anova(fuels_lmer_1000)

#2001 - 2023 <.0001 ones
#0.7518 tens
#0.0228 huns
#0.0510 thou




fuels_lmer_One_emm <- emmeans(fuels_lmer_One, ~year)
#estimated marginal mean - main effect taking into account marginal effects
contrast(fuels_lmer_One_emm, method="pairwise")
#contrast compares everything to everything

fuels_lmer_Ten_emm <- emmeans(fuels_lmer_Ten, ~year)
contrast(fuels_lmer_Ten_emm, method="pairwise")

fuels_lmer_Hun_emm <- emmeans(fuels_lmer_Hun, ~year)
contrast(fuels_lmer_Hun_emm, method="pairwise")

fuels_lmer_1000_emm <- emmeans(fuels_lmer_1000, ~year)
contrast(fuels_lmer_1000_emm, method="pairwise")


plot(fuels_lmer_One) #hoping to see even distribution above and below 0 line, looking for patterns, cone shaped? any patterns? want a scatter 

plot(fuels_lmer_Ten)

plot(fuels_lmer_Hun)


qqnorm(residuals(fuels_lmer_One))
qqline(residuals(fuels_lmer_One), col = "blue")

qqnorm(residuals(fuels_lmer_Ten))
qqline(residuals(fuels_lmer_Ten), col = "blue")

qqnorm(residuals(fuels_lmer_Hun))
qqline(residuals(fuels_lmer_Hun), col = "blue")


fuels_lmer_One_emm <- emmeans(fuels_lmer_One, ~year)
contrast(fuels_lmer_One_emm, "pairwise" , infer=TRUE, conf.int=TRUE)

kableone=tidy(contrast(fuels_lmer_One_emm, "pairwise", infer=TRUE), conf.int=TRUE)
kableone$Size_class="One"

tidy(contrast(fuels_lmer_One_emm, "pairwise", infer=TRUE), conf.int=TRUE) %>%
  select(Contrast=contrast, Estimate=estimate,
         SE=std.error, df=df, `CI-low`=conf.low, `CI-high`=conf.high,
         `P-value`='adj.p.value') %>%
  kable(digits=c(2, 2, 2, 2, 2, 2, 6), booktabs=T)


fuels_lmer_Ten_emm <- emmeans(fuels_lmer_Ten, ~year)
contrast(fuels_lmer_Ten_emm, "pairwise" , infer=TRUE, conf.int=TRUE)

kableten=tidy(contrast(fuels_lmer_Ten_emm, "pairwise", infer=TRUE), conf.int=TRUE)
kableten$Size_class="Ten"

tidy(contrast(fuels_lmer_Ten_emm, "pairwise", infer=TRUE), conf.int=TRUE) %>%
  select(Contrast=contrast, Estimate=estimate,
         SE=std.error, df=df, `CI-low`=conf.low, `CI-high`=conf.high,
         `P-value`='adj.p.value') %>%
  kable(digits=c(2, 2, 2, 2, 2, 2, 6), booktabs=T)


fuels_lmer_Hun_emm <- emmeans(fuels_lmer_Hun, ~year)
contrast(fuels_lmer_Hun_emm, "pairwise" , infer=TRUE, conf.int=TRUE)

kablehun=tidy(contrast(fuels_lmer_Hun_emm, "pairwise", infer=TRUE), conf.int=TRUE)
kablehun$Size_class="Hun"

tidy(contrast(fuels_lmer_Hun_emm, "pairwise", infer=TRUE), conf.int=TRUE) %>%
  select(Contrast=contrast, Estimate=estimate,
         SE=std.error, df=df, `CI-low`=conf.low, `CI-high`=conf.high,
         `P-value`='adj.p.value') %>%
  kable(digits=c(2, 2, 2, 2, 2, 2, 6), booktabs=T)


sum_table=rbind(kableone,kableten,kablehun)


ggplot(sum_table, aes(Size_class, contrast)) +
  geom_tile(aes(fill = adj.p.value), colour = "white") +
  scale_fill_gradient(low = "steelblue", high = "white")




