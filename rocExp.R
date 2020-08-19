library(ggplot2)
library(ggridges)

salsa <- read.csv(file="~/CurrentLx/newcastleModules/MRes/salsabila/salsaData.csv", header=T)

salsa <- subset(salsa, Age != "" && Age != "unknown")

salsa <- subset(salsa, Clump.Even != "")

salsa <- droplevels(salsa)

salsa$Age <- as.numeric(as.character(salsa$Age))

salsa$TimeToCompletion <- as.numeric(as.character(salsa$TimeToCompletion))


#box plots
ggplot(salsa, aes(Clump.Even, R, group=Clump.Even)) + 
  scale_y_continuous(name = "R") + 
  scale_x_discrete(name = "\nExperimental Condition") + 
  geom_point(alpha = 1/25) + 
  geom_boxplot(fill=c("purple","green")) +
  #geom_jitter(width = 0.3) +
  theme_bw() + 
  theme(panel.border = element_blank())

ggplot(salsa, aes(Clump.Even, TimeToCompletion, group=Clump.Even)) + 
  scale_y_continuous(name = "Time To Completion (mins)") + 
  scale_x_discrete(name = "\nExperimental Condition") + 
  geom_point(alpha = 1/25) + 
  geom_boxplot(fill=c("purple","green")) +
  #geom_jitter(width = 0.3) +
  theme_bw() + 
  theme(panel.border = element_blank())


#ridge plots
ggplot(salsa, aes(R, Clump.Even)) + 
  labs(y = "Condition", x = "\nR") +
  theme_bw() + theme(panel.border = element_blank())



ggplot(salsa, aes(Age, R, color=Clump.Even)) + 
  labs(y = "R", x = "\nAge") +
  #  geom_line() +
  geom_point() +
  stat_smooth()
scale_color_brewer(palette = "Set1") + 
  theme_bw() + theme(panel.border = element_blank())

ggplot(salsa, aes(Age, dprime, color=Clump.Even)) + 
  labs(y = "dprime", x = "\nAge") +
  #  geom_line() +
  geom_point() +
  stat_smooth()
scale_color_brewer(palette = "Set1") + 
  theme_bw() + theme(panel.border = element_blank())

ggplot(salsa, aes(Age, TimeToCompletion, color=Clump.Even)) + 
  labs(y = "Time To Completion", x = "\nAge") +
  #  geom_line() +
  geom_point() +
  stat_smooth()
scale_color_brewer(palette = "Set1") + 
  theme_bw() + theme(panel.border = element_blank())



salsa.Rfit.Gender.Age.Start.Clump.ClumpAge <- glm(R~Gender+Age+Bilingual+Low.Start.High.Start+Clump.Even+Clump.Even:Age, family = gaussian, data=salsa)

salsa.dprimefit.Gender.Age.Start.Clump.ClumpAge <- glm(dprime~Gender+Age+Bilingual+Low.Start.High.Start+Clump.Even+Clump.Even:Age, family = gaussian, data=salsa)

salsa.timefit.Gender.Age.Start.Clump.ClumpAge <- glm(TimeToCompletion~Gender+Age+Bilingual+Low.Start.High.Start+Clump.Even+Clump.Even:Age, family = gaussian, data=salsa)


salsa.Rfit.Gender.Age.Start.Clump <- glm(R~Gender+Age+Bilingual+Low.Start.High.Start+Clump.Even, family = gaussian, data=salsa)
salsa.Rfit.Gender.Age.Start <- glm(R~Gender+Age+Bilingual+Low.Start.High.Start, family = gaussian, data=salsa)

anova(salsa.Rfit.Gender.Age.Start.Clump.ClumpAge,salsa.Rfit.Gender.Age.Start.Clump, test="Chisq")
anova(salsa.Rfit.Gender.Age.Start.Clump,salsa.Rfit.Gender.Age.Start, test="Chisq")
AIC(salsa.Rfit.Gender.Age.Start)
AIC(salsa.Rfit.Gender.Age.Start.Clump)

salsa.timefit.Gender.Age.Start.Clump <- glm(TimeToCompletion~Gender+Age+Bilingual+Low.Start.High.Start+Clump.Even, family = gaussian, data=salsa)
salsa.timefit.Gender.Age.Start <- glm(TimeToCompletion~Gender+Age+Bilingual+Low.Start.High.Start, family = gaussian, data=salsa)

anova(salsa.timefit.Gender.Age.Start,salsa.timefit.Gender.Age.Start.Clump, test = "Chisq")
AIC(salsa.timefit.Gender.Age.Start)
AIC(salsa.timefit.Gender.Age.Start.Clump)