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
results <- read.csv(file="~/constantentropy/memory/dataAug2021.csv", header=T)



####box plots
foo <- ggplot(results, aes(OrderCondition, R, group=OrderCondition)) + 
  scale_y_continuous(name = "Recollection Parameter") + 
  scale_x_discrete(name = "\nExperimental Condition") + 
  geom_point(alpha = 1/25) + 
  geom_boxplot(fill=c("purple","green","purple","green","purple","green")) +
  geom_jitter(width = 0.2) +
  facet_wrap(~WordFreq) +
  theme_bw() + 
  theme(panel.border = element_blank())

###XXXX GGOT UP TO HERE REVISING XXXX

ggsave(foo, file = "~/memory/recolAllHighLow.pdf", width = 8.09, height = 5)

foo <- ggplot(results, aes(OrderCondition, dprime, group=OrderCondition)) + 
  scale_y_continuous(name = "Familiarity Parameter (d\')") + 
  scale_x_discrete(name = "\nExperimental Condition") + 
  geom_point(alpha = 1/25) + 
  geom_boxplot(fill=c("purple","green","purple","green","purple","green")) +
  geom_jitter(width = 0.2) +
  facet_wrap(~WordFreq) +
  theme_bw() + 
  theme(panel.border = element_blank())

ggsave(foo, file = "~/memory/dprimeAllHighLow.pdf", width = 8.09, height = 5)


foo <- ggplot(results, aes(OrderCondition, AUROC, group=OrderCondition)) + 
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



# Some initial processing. First of all, logs can't cope with 0s so replace these:
cutoff = 1e-2
results$dprime[results$dprime<cutoff] = cutoff
results$R[results$R<cutoff] = cutoff

# Make a normalised AUROC
results$AUROCnorm = 2*(results$AUROC-0.5)
results$AUROCnorm[results$AUROCnorm<0]=0.1

results_AllFreqOnly <- subset(results,WordFreq == "AllFreq")
results_AllFreqOnly <- droplevels(results_AllFreqOnly)


####Density plots of R and dprime, making the argument that we should use gamma models 
ggplot(results) + geom_density(aes(x=AUROCnorm, color=WordFreq)) +
  scale_color_brewer(palette = "Set1") + 
  theme_bw() + 
  theme(panel.border = element_blank())

####Shapiro-Wilk test for normality
shapiro.test(results_AllFreqOnly$AUROCnorm)$p.value
shapiro.test(results_AllFreqOnly$dprime)$p.value
shapiro.test(results_AllFreqOnly$R)$p.value
shapiro.test(results[results$WordFreq == "LoFreq",]$AUROC)$p.value

#Calculate 3rd moment with sample means, just to confirm positive skew
mean(((results_AllFreqOnly$R - mean(results_AllFreqOnly$R))/sd(results_AllFreqOnly$R))^3)


####Simple Models with all frequency data together

#Modeling R with demographics, TimeToCompletion, and experimental conditions
Rfit.Gender.Age.Bilingual.Time.StartCondition.OrderCondition <- glm(R~Gender+Age+Bilingual+StartCondition+OrderCondition+TimeToCompletion, family = Gamma(link = log), data=results_AllFreqOnly)
summary(Rfit.Gender.Age.Bilingual.Time.StartCondition.OrderCondition)

#Note that we did test for a significant interaction of TimeToCompletion and OrderCondition, but it did not significantly improve model fit (p = 0.61)
Rfit.Gender.Age.Bilingual.StartCondition.TimeXOrderCondition <- glm(R~Gender+Age+Bilingual+StartCondition+OrderCondition*TimeToCompletion, family = Gamma(link = log), data=results_AllFreqOnly)
anova(Rfit.Gender.Age.Bilingual.Time.StartCondition.OrderCondition, Rfit.Gender.Age.Bilingual.StartCondition.TimeXOrderCondition, test="Chisq")

#Model without OrderCondition for comparison
Rfit.Gender.Age.Bilingual.Time.StartCondition <- glm(R~Gender+Age+Bilingual+StartCondition+TimeToCompletion, family = Gamma(link = log), data=results_AllFreqOnly)
summary(Rfit.Gender.Age.Bilingual.Time.StartCondition)

anova(Rfit.Gender.Age.Bilingual.Time.StartCondition.OrderCondition, Rfit.Gender.Age.Bilingual.Time.StartCondition, test="Chisq")
AIC(Rfit.Gender.Age.Bilingual.Time.StartCondition)
BIC(Rfit.Gender.Age.Bilingual.Time.StartCondition)
AIC(Rfit.Gender.Age.Bilingual.Time.StartCondition.OrderCondition)
BIC(Rfit.Gender.Age.Bilingual.Time.StartCondition.OrderCondition)


#Modeling dprime with demographics, TimeToCompletion, and experimental conditions
dprimeFit.Gender.Age.Bilingual.Time.StartCondition.OrderCondition <- glm(dprime~Gender+Age+Bilingual+StartCondition+OrderCondition+TimeToCompletion, family = Gamma(link = log), data=results_AllFreqOnly)
summary(dprimeFit.Gender.Age.Bilingual.Time.StartCondition.OrderCondition)

#Model without OrderCondition for comparison
dprimeFit.Gender.Age.Bilingual.Time.StartCondition <- glm(dprime~Gender+Age+Bilingual+StartCondition+TimeToCompletion, family = Gamma(link = log), data=results_AllFreqOnly)
summary(dprimeFit.Gender.Age.Bilingual.Time.StartCondition)

anova(dprimeFit.Gender.Age.Bilingual.Time.StartCondition.OrderCondition, dprimeFit.Gender.Age.Bilingual.Time.StartCondition, test="Chisq")
AIC(dprimeFit.Gender.Age.Bilingual.Time.StartCondition)
BIC(dprimeFit.Gender.Age.Bilingual.Time.StartCondition)
AIC(dprimeFit.Gender.Age.Bilingual.Time.StartCondition.OrderCondition)
BIC(dprimeFit.Gender.Age.Bilingual.Time.StartCondition.OrderCondition)


#Modeling AUROC with demographics, TimeToCompletion, and experimental conditions
AUROCfit.Gender.Age.Bilingual.Time.StartCondition.OrderCondition <- glm(AUROCnorm~Gender+Age+Bilingual+StartCondition+OrderCondition+TimeToCompletion, family = Gamma(link = log), data=results_AllFreqOnly)
summary(AUROCfit.Gender.Age.Bilingual.Time.StartCondition.OrderCondition)

#Model without OrderCondition for comparison
AUROCfit.Gender.Age.Bilingual.Time.StartCondition <- glm(AUROCnorm~Gender+Age+Bilingual+StartCondition+TimeToCompletion, family = Gamma(link = log), data=results_AllFreqOnly)
summary(AUROCfit.Gender.Age.Bilingual.Time.StartCondition)

anova(AUROCfit.Gender.Age.Bilingual.Time.StartCondition.OrderCondition, AUROCfit.Gender.Age.Bilingual.Time.StartCondition, test="Chisq")
AIC(AUROCfit.Gender.Age.Bilingual.Time.StartCondition)
BIC(AUROCfit.Gender.Age.Bilingual.Time.StartCondition)
AIC(AUROCfit.Gender.Age.Bilingual.Time.StartCondition.OrderCondition)
BIC(AUROCfit.Gender.Age.Bilingual.Time.StartCondition.OrderCondition)


########Models with WordFreq data separated by high and low
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
