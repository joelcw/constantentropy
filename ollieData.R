ollie.df <- read.delim(file="~/CurrentLx/infoTheory/ollie/tweets_202005121803.csv", sep = "\t",header = T)

ollie.df$parents <- as.numeric(as.character(ollie.df$parents))
ollie.df$dorm <- as.double(as.character(ollie.df$dorm))
ollie.df$reply_to_self <- as.factor(as.numeric(as.character(ollie.df$reply_to_self)))


ollie.fit.PR <- lm(dorm~parents*reply_to_self, data=ollie.df)

summary(ollie.fit.PR)

ollie.fit.P.R <- lm(dorm~parents+reply_to_self, data=ollie.df)

#Interpretation of the interaction below: having parent nodes decreases DORM even more if the tweets are NOT a reply_to_self.
anova(ollie.fit.PR,ollie.fit.P.R)

ollie.fit.P <- lm(dorm~parents, data=ollie.df)

anova(ollie.fit.P,ollie.fit.P.R)

ollie.fit.R <- lm(dorm~reply_to_self, data=ollie.df)

library(ggplot2)
library(splines)
library(MASS)

#Tried the various options below: splines, polynomials
p <- ggplot(ollie.chopped.df, aes(parents, dorm, color=Bot)) + 
  labs(y = "DORM", x = "\nParents") +
#  geom_line() +
  geom_point() +
  stat_smooth(method= "lm", formula = y ~ splines::bs(x, 3)) +  #x + I(x^2) + I(x^3) 
  scale_color_brewer(palette = "Set1") + 
  theme_bw() + theme(panel.border = element_blank())

ggsave(p, file = "PRinteraction_spline10parents.png", width = 8.09, height = 5)


#Just checking to see that the dorms are normally distributed
ggplot(ollie.df) + 
  geom_density(aes(x=dorm))


#shapiro-wilk test for normality on a random sample of 5000 observations (cause the test can't deal with more)
shapiro.test(sample(ollie.df$dorm, 5000, replace = F))

#try Zing parents
ollie.df$zParents <- scale(ollie.df$parents, center=TRUE, scale=TRUE)

ollie.fit.PR <- lm(dorm~zParents*reply_to_self, data=ollie.df)

summary(ollie.fit.PR)

ollie.fit.P.R <- lm(dorm~zParents+reply_to_self, data=ollie.df)

#Interpretation of the interaction below: having parent nodes decreases DORM even more if the tweets are NOT a reply_to_self.
anova(ollie.fit.PR,ollie.fit.P.R)

ollie.fit.P <- lm(dorm~zParents, data=ollie.df)

anova(ollie.fit.P,ollie.fit.P.R)

#chop off the parent tail
ollie.chopped.df <- subset(ollie.df,parents <= 10)
ollie.chopped.df <- droplevels(ollie.chopped.df)

ollie.chopped.fit.PR <- lm(dorm~parents*reply_to_self, data=ollie.chopped.df)

summary(ollie.chopped.fit.PR)

ollie.chopped.fit.P.R <- lm(dorm~parents+reply_to_self, data=ollie.chopped.df)

#Interpretation of the interaction below: having parent nodes decreases DORM even more if the tweets are NOT a reply_to_self.
anova(ollie.chopped.fit.PR,ollie.chopped.fit.P.R)

ollie.chopped.fit.P <- lm(dorm~parents, data=ollie.chopped.df)

anova(ollie.chopped.fit.P,ollie.chopped.fit.P.R)


#trying model with higher order terms
ollie.fit.cube.PR <- glm(dorm~parents + reply_to_self + I(parents^2) + I(parents^3) + parents:reply_to_self + I(parents^2):reply_to_self + I(parents^3):reply_to_self, family = gaussian, data=ollie.df)
ollie.fit.quad.PR <- glm(dorm~parents + reply_to_self + I(parents^2) + parents:reply_to_self + I(parents^2):reply_to_self, family = gaussian, data=ollie.df)
AIC(ollie.fit.cube.PR)-AIC(ollie.fit.quad.PR) # cube is better

summary(ollie.fit.cube.PR)

ollie.fit.cube.P.R <- glm(dorm~parents + reply_to_self + I(parents^2) + I(parents^3), family = gaussian, data=ollie.df)

AIC(ollie.fit.cube.PR)-AIC(ollie.fit.cube.P.R)

#Interpretation of the interaction below: having parent nodes decreases DORM even more if the tweets are NOT a reply_to_self.
anova(ollie.fit.cube.PR,ollie.fit.cube.P.R,test="Chisq")

ollie.fit.cube.P <- glm(dorm~parents + I(parents^2) + I(parents^3), family = gaussian, data=ollie.df)
AIC(ollie.fit.cube.P.R)-AIC(ollie.fit.cube.P) #cube with R is minimally better, by like 1.7

ollie.df$reply_to_self <- as.numeric(as.character(ollie.df$reply_to_self))

ollie.fit.cube.R <- glm(dorm~reply_to_self + I(reply_to_self^2) + I(reply_to_self^3), family = gaussian, data=ollie.df)
AIC(ollie.fit.cube.P.R)-AIC(ollie.fit.cube.R) #cube with P is tons better than linear R (and cubic R isn't possible, or wouldnt make a difference cause it's binary)

summary(ollie.fit.cube.P.R)

anova(ollie.fit.cube.R,ollie.fit.cube.P.R,test="Chisq")

#bots data

bots.df <- read.delim(file="~/CurrentLx/infoTheory/ollie/botTweets_202006211624.csv", sep = "\t",header = T)

bots.df$parents <- as.numeric(as.character(bots.df$parents))
bots.df$dorm <- as.double(as.character(bots.df$dorm))
bots.df$reply_to_self <- as.factor(as.numeric(as.character(bots.df$reply_to_self)))


bots.df$Bot <- 1

ollie.df$Bot <- 0

ollie.bots.df <- rbind(ollie.df,bots.df)

ollie.bots.df$Bot <- as.numeric(as.character(ollie.bots.df$Bot))

ggplot(ollie.bots.df, aes(parents, dorm, color=Bot)) + 
  labs(y = "DORM", x = "\nParents") +
  #  geom_line() +
  geom_point() +
  stat_smooth()
  scale_color_brewer(palette = "Set1") + 
  theme_bw() + theme(panel.border = element_blank())

#predicting bot or not

ollie.bots.DP.R <- glm(Bot~dorm+parents+reply_to_self+dorm:parents, family = binomial, data=ollie.bots.df)
summary(ollie.bots.DP.R)

ollie.bots.D.P.R <- glm(Bot~dorm+parents+reply_to_self, family = binomial, data=ollie.bots.df)

anova(ollie.bots.D.P.R,ollie.bots.DP.R,test="Chisq")
AIC(ollie.bots.D.P.R)
AIC(ollie.bots.DP.R)

ollie.bots.P.R <- glm(Bot~parents+reply_to_self, family = binomial, data=ollie.bots.df)

anova(ollie.bots.D.P.R, ollie.bots.P.R,test="Chisq")
AIC(ollie.bots.D.P.R)
AIC(ollie.bots.P.R)

summary(ollie.bots.D.P.R)

ollie.bots.D.P <- glm(Bot~dorm+parents, family = binomial, data=ollie.bots.df)
anova(ollie.bots.D.P, ollie.bots.D.P.R, test="Chisq")
AIC(ollie.bots.D.P.R)
AIC(ollie.bots.D.P)

ollie.bots.D.cubeP <- glm(Bot~dorm+parents + I(parents^2) + I(parents^3), family = binomial, data=ollie.bots.df)
anova(ollie.bots.D.P, ollie.bots.D.cubeP, test="Chisq")
AIC(ollie.bots.D.cubeP)
AIC(ollie.bots.D.P) #cubic one does fit better

ollie.bots.cubeP <- glm(Bot~parents + I(parents^2) + I(parents^3), family = binomial, data=ollie.bots.df)
anova(ollie.bots.cubeP, ollie.bots.D.cubeP, test="Chisq")

ollie.bots.DcubeP <- glm(Bot~dorm+parents + I(parents^2) + I(parents^3) + dorm:parents + dorm:I(parents^2) + dorm:I(parents^3), family = binomial, data=ollie.bots.df)
anova(ollie.bots.DcubeP, ollie.bots.D.cubeP, test="Chisq")
AIC(ollie.bots.D.cubeP)
AIC(ollie.bots.DcubeP) #cubic one with interaction just barely does better; AIc diff = 4, p = 0.0169

summary(ollie.bots.DcubeP)

#parents > 0, assuming that bots rarely reply in/to long threads. This should lower the number of bots hiding in the non-bot dataset
ollie.bots1.df <- subset(ollie.bots.df,parents > 0)
ollie.bots1.df <- droplevels(ollie.bots1.df)

ollie.bots1.DcubeP <- glm(Bot~dorm+parents + I(parents^2) + I(parents^3) + dorm:parents + dorm:I(parents^2) + dorm:I(parents^3), family = binomial, data=ollie.bots1.df)
summary(ollie.bots1.DcubeP)

ollie.bots1.D.cubeP <- glm(Bot~dorm+parents + I(parents^2) + I(parents^3), family = binomial, data=ollie.bots1.df)

ollie.bots1.cubeP <- glm(Bot~parents + I(parents^2) + I(parents^3), family = binomial, data=ollie.bots1.df)
anova(ollie.bots1.cubeP, ollie.bots1.D.cubeP, test="Chisq")
