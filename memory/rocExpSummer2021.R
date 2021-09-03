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
results$TimeToCompletion <- as.numeric(as.character(results$TimeToCompletion))

# Some initial processing. First of all, logs can't cope with 0s so replace these:
cutoff = 1e-2
results$dprime[results$dprime<cutoff] = cutoff
results$R[results$R<cutoff] = cutoff

# Make a normalised AUROC
results$AUROCnorm = 2*(results$AUROC-0.5)
results$AUROCnorm[results$AUROCnorm<0]=0.1

results_AllFreqOnly <- subset(results,WordFreq == "AllFreq")
results_AllFreqOnly <- droplevels(results_AllFreqOnly)


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

results_highLow <- subset(results,WordFreq != "AllFreq")
results_highLow <- droplevels(results_highLow)

#Modeling R with word frequency, OrderCondition, and random slopes by participant. The mixed effects model will not converge if TimetoCompletion is included.
Rfit.OrderCondition.WordFreq <- glmer(R~(1|ParticipantIdentifier)+OrderCondition+WordFreq, family = Gamma(link = log), data=results_highLow)
summary(Rfit.OrderCondition.WordFreq)

Rfit.OrderConditionXwordFreq <- glmer(R~(1|ParticipantIdentifier)+OrderCondition*WordFreq, family = Gamma(link = log), data=results_highLow)
summary(Rfit.OrderConditionXwordFreq)

anova(Rfit.OrderCondition.WordFreq, Rfit.OrderConditionXwordFreq, test="Chisq")

#Model without OrderCondition for comparison
Rfit.WordFreq <- glmer(R~(1|ParticipantIdentifier)+WordFreq, family = Gamma(link = log), data=results_highLow)
summary(Rfit.WordFreq)

anova(Rfit.OrderCondition.WordFreq, Rfit.WordFreq, test="Chisq")

#Non-hierarchical model with WordFreq
Rfit.Time.OrderCondition.WordFreq <- glm(R~OrderCondition+TimeToCompletion+WordFreq, family = Gamma(link = log), data=results_highLow)
summary(Rfit.Time.OrderCondition.WordFreq)


#The mixed effects model will not converge for dprime.
dprimeFit.OrderCondition.WordFreq <- glmer(dprime~(1|ParticipantIdentifier)+OrderCondition+WordFreq, family = Gamma(link = log), data=results_highLow)
summary(dprimeFit.OrderCondition.WordFreq)


#Non-hierarchical models for dprime with WordFreq
dprimeFit.Time.OrderCondition.WordFreq <- glm(dprime~OrderCondition+WordFreq+TimeToCompletion, family = Gamma(link = log), data=results_highLow)
summary(dprimeFit.Time.OrderCondition.WordFreq)

dprimeFit.Time.OrderConditionXwordFreq <- glm(dprime~OrderCondition*WordFreq+TimeToCompletion, family = Gamma(link = log), data=results_highLow)

anova(dprimeFit.Time.OrderCondition.WordFreq,dprimeFit.Time.OrderConditionXwordFreq, test="Chisq")
AIC(dprimeFit.Time.OrderCondition.WordFreq)
AIC(dprimeFit.Time.OrderConditionXwordFreq)
BIC(dprimeFit.Time.OrderCondition.WordFreq)
BIC(dprimeFit.Time.OrderConditionXwordFreq)


#Mixed effects model for AUROCnorm 
AUROCfit.OrderCondition.WordFreq <- glmer(AUROCnorm~(1|ParticipantIdentifier)+OrderCondition+WordFreq+TimeToCompletion, family = Gamma(link = log), data=results_highLow)
summary(AUROCfit.OrderCondition.WordFreq)

#Non-hierarchical model for AUROCnorm 
AUROCfit.OrderCondition.WordFreq.Time <- glm(AUROCnorm~OrderCondition+WordFreq+TimeToCompletion, family = Gamma(link = log), data=results_highLow)
summary(AUROCfit.OrderCondition.WordFreq.Time)


