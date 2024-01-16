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

seedlings <- read_csv("C:/Users/edeegan/OneDrive - DOI/Fire_project/Fire_project/PMSE_data/PSME_Trees - Seedlings (Height Class) (metric)_XPT.csv")
seedlings<-as_tibble(seedlings)
seedlings=rename(seedlings, plot=`MacroPlot Name`)

seedlings=seedlings %>% separate(Date, c("month", "day", "year"), "/")
seedlings=seedlings %>% separate(year, c("year"), " ")
#Fill in the missing plot names, year, and/or species and insert a “zero” for Count

seedlings_fill <- seedlings %>% drop_na(Count)

#Convert “plot” and “year” to factors with mutate. In R, factors are variables that take on a limited number
#of different values; often referred to as categorical variables (e.g. not continuous). Categorical variables
#enter into statistical models differently than continuous variables, so storing data as factors ensures that the
#modeling functions will treat such data correctly.

seedlings_factor1 <- seedlings_fill %>%
  mutate(plot=factor(plot))

seedlings_factor2 <- seedlings_factor1 %>%
  mutate(year=factor(year))


#Calculate summary statistics. Create a table with sample size, minimum value, maximum value, mean,
#standard deviation, and standard error. Use kable for an improved table.

seedlings_factor2_summ <- seedlings_factor2 %>%
  group_by(`Year`=year) %>%
  summarise(n=n(),
            `Min.`=min(Count),
            `Max.`=max(Count),
            `Mean`=mean(Count),
            `SD`=sd(Count),
            `SE`=sd(Count)/sqrt(n))

kable(seedlings_factor2_summ, booktabs=T, digits=3)

#Create a boxplot. A boxplot displays the distribution of a dataset based on its five number summary of data
#points: minimum, 1st quartile (25th percentile), median, third quartile (75th percentile), and maximum. A
#boxplot can be used to show the symmetry, skew, variance, and outliers of a dataset.

ggplot() +
  theme_bw() +
  geom_boxplot(aes(y=Count, x=year),
               data=seedlings_factor2) +
  geom_jitter(aes(y=Count, x=year),
              height=0,
              data=seedlings_factor2) +
  ylab("Average seedlings Count ") +
  ggtitle("Average seedlings Count ")


#Create a histogram. A histogram is an approximate representation of the distribution of continuous data.
#Histograms can be used to identify patterns in data, such as the shape of the distribution (e.g. normally
# distributed, skewed), the spread of the data, and outliers. The height of each bar represents the frequency
#(count) of data points within the corresponding bin (x-axis).


ggplot() +
  geom_histogram(aes(x=Count),
                 bins=4,
                 data=seedlings_factor2) +
  xlab("Average seedlings Count ") +
  ggtitle("Average seedlings Count ")


#Fit a mixed model for a repeated measures design using lmer and indicate “plot” as a random effect. Call
#summary on the model. We have repeated measurements on individual plots (experimental units) and those
#measurements will be correlated (not independent). A mixed model accounts for the correlated responses.
#We indicate “plot” as a random effect to account for the correlation between measurements that arise from
#the same plot. “Plot” has random variation and is not of primary interest in this analysis.

seedlings_lmer <- lmer(Count~year + (1|plot),
                    data=seedlings_factor2)
summary(seedlings_lmer)


#Call anova (Analysis of Variance) on the model. Question: “Is there a difference in mean seedlings Count
#between years?”
#Null hypothesis (HO): There is no difference in mean seedlings Count between years.
#Alternative hypothesis (HA): There is a difference in mean seedlings Count between years.
#A small p-value (less than alpha 0.05) will reject the null hypothesis that there is no difference in mean
#seedlings Count between years. You can then conclude that there is evidence of a difference in mean seedlings
#Count between years.

anova(seedlings_lmer, ddf="Kenward-Roger")

#Create a plot of standardized residuals vs. fitted values for the model to assess the assumption of constant
#variance. Fitted values are the values predicted by the model. Residuals are the differences between the
#observed values (data) and the corresponding fitted values. This plot displays the fitted values of the model
#along the x-axis and the residuals of the fitted values along the y-axis. If the spread of the residuals is roughly
#equal at each level of the fitted values, the constant variance assumption is met. The residuals should be
#scattered randomly about zero, with no obvious pattern emerging

plot(seedlings_lmer)

#Create a Quantile-Quantile (QQ) plot to assess normality of the residuals (normal distribution). A QQ Plot
#compares two probability distributions by plotting their quantiles against each other: the quantiles of the
#sample data versus the theoretical quantile values from a normal distribution (or what we would expect from
# a normal distribution). Data points should fall on a fairly straight line to indicate linearity. If data points
#deviate largely from a straight line, it suggests that the two data sets do not have the same distribution.

qqnorm(residuals(seedlings_lmer))
qqline(residuals(seedlings_lmer), col = "blue")


#Create an emmeans object and conduct Tukey-adjusted pairwise comparisons (“contrasts”) between years.
#These contrasts provide estimates of the pairwise differences in average live seedlings Count between years.
#Include confidence intervals and p-values that have been adjusted for multiple comparisons.
#Multiple comparison problem: each time you run a hypothesis test, there is a small chance you will obtain
#a “false” significant result (you will reject the null hypothesis when it is actually true, also called a Type I
#Error Rate). If you run multiple tests, the number of “false positives” increases with each test, so to control
#this Type I Error Rate, the p-values can be adjusted (Tukey is one method and used here) to be more
#conservative (less false positives).
#Call tidy to create an emmeans table. Copy tidy code and include kable for an improved table. Use the
#table column headers in the first tidy table to create the headers in the second (and final) tidy table.

seedlings_lmer_emm <- emmeans(seedlings_lmer, ~year)
contrast(seedlings_lmer_emm, "pairwise" , infer=TRUE, conf.int=TRUE)

tidy(contrast(seedlings_lmer_emm, "pairwise", infer=TRUE), conf.int=TRUE)

tidy(contrast(seedlings_lmer_emm, "pairwise", infer=TRUE), conf.int=TRUE) %>%
  select(Contrast=contrast, Estimate=estimate,
         SE=std.error, df=df, `CI-low`=conf.low, `CI-high`=conf.high,
         `P-value`='adj.p.value') %>%
  kable(digits=c(2, 2, 2, 2, 2, 2, 6), booktabs=T)
#estimate of the difference of the means

#Eva's timeline graph
seedlings_factor2_summ %>%
  ggplot(aes(x=Year, y=Mean, size=Mean))+geom_point()+theme_classic()+ylab("Seedlings Count")

