library(ggplot2)
library(RColorBrewer)
library(ggridges)
library(ggbeeswarm)
library(tidyr)
library(dplyr)
library(lmerTest)
library(lmodel2)
library(lme4)

###Old stuff:
#salsaOld <- read.csv(file="~/constantentropy/memory/allWordsTogether.csv", header=T)
#salsa <- subset(salsa, WorkerType == "turker")
#highlowFullOld <- read.csv(file="~/constantentropy/memory/highlowFull.csv", header=T)


#####Current Dataset:
####Note that participants with TimetoCompletion < 12 or > 51 have been excluded, which are below 5th percentile and above the 95th percentile.
jennyData <- read.csv(file="~/constantentropy/memory/dataAug2021.csv", header=T)



####box plots
foo <- ggplot(jennyData, aes(OrderCondition, R, group=OrderCondition)) + 
  scale_y_continuous(name = "Recollection Parameter") + 
  scale_x_discrete(name = "\nExperimental Condition") + 
  geom_point(alpha = 1/25) + 
  geom_boxplot(fill=c("purple","green","purple","green","purple","green")) +
  #geom_jitter(width = 0.3) +
  facet_wrap(~WordFreq) +
  theme_bw() + 
  theme(panel.border = element_blank())

###XXXX GGOT UP TO HERE REVISING XXXX

ggsave(foo, file = "~/memory/recolAllHighLow.pdf", width = 8.09, height = 5)

foo <- ggplot(jennyData, aes(OrderCondition, dprime, group=OrderCondition)) + 
  scale_y_continuous(name = "Familiarity Parameter (d\')") + 
  scale_x_discrete(name = "\nExperimental Condition") + 
  geom_point(alpha = 1/25) + 
  geom_boxplot(fill=c("purple","green","purple","green","purple","green")) +
  #geom_jitter(width = 0.3) +
  facet_wrap(~WordFreq) +
  theme_bw() + 
  theme(panel.border = element_blank())

ggsave(foo, file = "~/memory/dprimeAllHighLow.pdf", width = 8.09, height = 5)


foo <- ggplot(jennyData, aes(OrderCondition, AUROC, group=OrderCondition)) + 
  scale_y_continuous(name = "Familiarity Parameter (d\')") + 
  scale_x_discrete(name = "\nExperimental Condition") + 
  geom_point(alpha = 1/25) + 
  geom_boxplot(fill=c("purple","green","purple","green","purple","green")) +
  #geom_jitter(width = 0.3) +
  facet_wrap(~WordFreq) +
  theme_bw() + 
  theme(panel.border = element_blank())

ggsave(foo, file = "~/memory/AUROCallHighLow.pdf", width = 8.09, height = 5)


###Same plots in other forms



####Models, all word freq data together
jennyData_AllFreqOnly <- subset(jennyData,WordFreq == "AllFreq")
jennyData_AllFreqOnly <- droplevels(jennyData_AllFreqOnly)


####Density plots of R and dprime, making the argument that we should use gamma models 
ggplot(jennyData) + geom_density(aes(x=dprime, color=WordFreq)) +
  scale_color_brewer(palette = "Set1") + 
  theme_bw() + 
  theme(panel.border = element_blank())

####Shapiro-Wilk test for normality
shapiro.test(jennyData_AllFreqOnly$AUROC)$p.value
shapiro.test(jennyData_AllFreqOnly$dprime)$p.value
shapiro.test(jennyData_AllFreqOnly$R)$p.value
shapiro.test(jennyData[jennyData$WordFreq == "LoFreq",]$AUROC)$p.value

#Calculate 3rd moment with sample means, just to confirm positive skew
mean(((jennyData_AllFreqOnly$R - mean(jennyData_AllFreqOnly$R))/sd(jennyData_AllFreqOnly$R))^3)

####XXXGOT UP TO HERE XXX


salsa.Rfit.Gender.Age.Start.Clump.ClumpAge <- glm(R~Gender+Age+Bilingual+Low.Start.High.Start+Clump.Even+Clump.Even:Age, family = gaussian, data=salsa)

salsa.dprimefit.Gender.Age.Start.Clump.ClumpAge <- glm(dprime~Gender+Age+Bilingual+Low.Start.High.Start+Clump.Even+Clump.Even:Age, family = gaussian, data=salsa)

salsa.timefit.Gender.Age.Start.Clump.ClumpAge <- glm(TimeToCompletion~Gender+Age+Bilingual+Low.Start.High.Start+Clump.Even+Clump.Even:Age, family = gaussian, data=salsa)


salsa.Rfit.Gender.Age.Start.Clump <- glm(R~Gender+Age+Bilingual+Low.Start.High.Start+Clump.Even, family = gaussian, data=salsa)
summary(salsa.Rfit.Gender.Age.Start.Clump)
salsa.Rfit.Gender.Age.Start <- glm(R~Gender+Age+Bilingual+Low.Start.High.Start, family = gaussian, data=salsa)

anova(salsa.Rfit.Gender.Age.Start.Clump.ClumpAge,salsa.Rfit.Gender.Age.Start.Clump, test="Chisq")
anova(salsa.Rfit.Gender.Age.Start.Clump,salsa.Rfit.Gender.Age.Start, test="Chisq")
AIC(salsa.Rfit.Gender.Age.Start)
BIC(salsa.Rfit.Gender.Age.Start)
AIC(salsa.Rfit.Gender.Age.Start.Clump)
BIC(salsa.Rfit.Gender.Age.Start.Clump)

salsa.auroc.Gender.Age.Start.Clump <- glm(AUROC~Gender+Age+Bilingual+Low.Start.High.Start+Clump.Even, family = gaussian, data=salsa)
summary(salsa.auroc.Gender.Age.Start.Clump)
salsa.auroc.Gender.Age.Start <- glm(AUROC~Gender+Age+Bilingual+Low.Start.High.Start, family = gaussian, data=salsa)
anova(salsa.auroc.Gender.Age.Start.Clump,salsa.auroc.Gender.Age.Start, test="Chisq")
AIC(salsa.auroc.Gender.Age.Start)
BIC(salsa.auroc.Gender.Age.Start)
AIC(salsa.auroc.Gender.Age.Start.Clump)
BIC(salsa.auroc.Gender.Age.Start.Clump)

salsa.dprime.Gender.Age.Start.Clump <- glm(dprime~Gender+Age+Bilingual+Low.Start.High.Start+Clump.Even, family = gaussian, data=salsa)
summary(salsa.dprime.Gender.Age.Start.Clump)
salsa.dprime.Gender.Age.Start <- glm(dprime~Gender+Age+Bilingual+Low.Start.High.Start, family = gaussian, data=salsa)
anova(salsa.dprime.Gender.Age.Start.Clump,salsa.dprime.Gender.Age.Start, test="Chisq")
AIC(salsa.dprime.Gender.Age.Start)
BIC(salsa.dprime.Gender.Age.Start)
AIC(salsa.dprime.Gender.Age.Start.Clump)
BIC(salsa.dprime.Gender.Age.Start.Clump)


salsa.timefit.Gender.Age.Start.Clump <- glm(TimeToCompletion~Gender+Age+Bilingual+Low.Start.High.Start+Clump.Even, family = gaussian, data=salsa)
salsa.timefit.Gender.Age.Start <- glm(TimeToCompletion~Gender+Age+Bilingual+Low.Start.High.Start, family = gaussian, data=salsa)

anova(salsa.timefit.Gender.Age.Start,salsa.timefit.Gender.Age.Start.Clump, test = "Chisq")
AIC(salsa.timefit.Gender.Age.Start)
AIC(salsa.timefit.Gender.Age.Start.Clump)


########Models, word freq data separated by high and low
library(lme4)
library(lmerTest)
highlowFull.Rfit.Sex.Age.Start.Clump.Freq.FreqClump <- lmer(R~(1|Participant)+Sex+Age+Bilingual+LowOrHighStart+ClumpedOrEven+Frequency+Frequency:ClumpedOrEven, data=highlowFull)
highlowFull.Rfit.Sex.Age.Start.Clump.Freq <- lmer(R~(1|Participant)+Sex+Age+Bilingual+LowOrHighStart+ClumpedOrEven+Frequency, data=highlowFull)
summary(highlowFull.Rfit.Sex.Age.Start.Clump.Freq)
anova(highlowFull.Rfit.Sex.Age.Start.Clump.Freq.FreqClump,highlowFull.Rfit.Sex.Age.Start.Clump.Freq, test="Chisq")
AIC(highlowFull.Rfit.Sex.Age.Start.Clump.Freq.FreqClump)
AIC(highlowFull.Rfit.Sex.Age.Start.Clump.Freq)
BIC(highlowFull.Rfit.Sex.Age.Start.Clump.Freq.FreqClump)
BIC(highlowFull.Rfit.Sex.Age.Start.Clump.Freq)

highlowFull.Rfit.Sex.Age.Start.Clump <- lmer(R~(1|Participant)+Sex+Age+Bilingual+LowOrHighStart+ClumpedOrEven, data=highlowFull)
highlowFull.Rfit.Sex.Age.Start.Freq <- lmer(R~(1|Participant)+Sex+Age+Bilingual+LowOrHighStart+Frequency, data=highlowFull)
anova(highlowFull.Rfit.Sex.Age.Start.Freq,highlowFull.Rfit.Sex.Age.Start.Clump.Freq, test="Chisq")
anova(highlowFull.Rfit.Sex.Age.Start.Clump,highlowFull.Rfit.Sex.Age.Start.Clump.Freq, test="Chisq")

#same for dprime, ie familiarity
highlowFull.dfit.Sex.Age.Start.Clump.Freq.FreqClump <- lmer(dprime~(1|Participant)+Sex+Age+Bilingual+LowOrHighStart+ClumpedOrEven+Frequency+Frequency:ClumpedOrEven, data=highlowFull)
highlowFull.dfit.Sex.Age.Start.Clump.Freq <- lmer(dprime~(1|Participant)+Sex+Age+Bilingual+LowOrHighStart+ClumpedOrEven+Frequency, data=highlowFull)
summary(highlowFull.dfit.Sex.Age.Start.Clump.Freq.FreqClump)
anova(highlowFull.dfit.Sex.Age.Start.Clump.Freq.FreqClump,highlowFull.dfit.Sex.Age.Start.Clump.Freq, test="Chisq")
AIC(highlowFull.dfit.Sex.Age.Start.Clump.Freq.FreqClump)
AIC(highlowFull.dfit.Sex.Age.Start.Clump.Freq)
BIC(highlowFull.dfit.Sex.Age.Start.Clump.Freq.FreqClump)
BIC(highlowFull.dfit.Sex.Age.Start.Clump.Freq)

#same for AUROC
highlowFull.auroc.Sex.Age.Start.Clump.Freq.FreqClump <- lmer(AUROC~(1|Participant)+Sex+Age+Bilingual+LowOrHighStart+ClumpedOrEven+Frequency+Frequency:ClumpedOrEven, data=highlowFull)
highlowFull.auroc.Sex.Age.Start.Clump.Freq <- lmer(AUROC~(1|Participant)+Sex+Age+Bilingual+LowOrHighStart+ClumpedOrEven+Frequency, data=highlowFull)
highlowFull.auroc.Sex.Age.Start.Freq <- lmer(AUROC~(1|Participant)+Sex+Age+Bilingual+LowOrHighStart+Frequency, data=highlowFull)
summary(highlowFull.auroc.Sex.Age.Start.Clump.Freq)
anova(highlowFull.auroc.Sex.Age.Start.Clump.Freq,highlowFull.auroc.Sex.Age.Start.Clump.Freq.FreqClump, test="Chisq")
AIC(highlowFull.auroc.Sex.Age.Start.Clump.Freq.FreqClump)
AIC(highlowFull.auroc.Sex.Age.Start.Clump.Freq)
BIC(highlowFull.auroc.Sex.Age.Start.Clump.Freq.FreqClump)
BIC(highlowFull.auroc.Sex.Age.Start.Clump.Freq)
