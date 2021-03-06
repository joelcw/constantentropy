library(ggplot2)
library(plyr)

####For English data.
####Read the file of CorpusSearch codes into an R data frame.

foo <- read.delim("~/constantentropy/outputs/infoTheoryTest.ymeb.cod.ooo",header=F,sep=":")


####Give appropriate column names to the columns

colnames(foo) <- c("OV","Clause","ObjType","SbjType","ID","Year")


####Throw out all the codes that refer to tokens that are irrelevant for the study.

"Got up to subsetting"

objsbj.data <- subset(foo, OV != "z" & Clause != "z" & Year != "0" & Year != "" & Year != "na" & SbjType != "z" & SbjType != "" & ObjType != "z" & ObjType != "" & ID != "")


library(gdata)


####Make sure R factor groups don't include factors for the irrelevant codes.

objsbj.data <- droplevels(objsbj.data)

"finished droplevels"

####Make sure dates abd 0/1 codes are stored as numbers, and weights

objsbj.data$Year <- as.numeric(as.character(objsbj.data$Year))
objsbj.data$OV <- as.numeric(as.character(objsbj.data$OV))

"finished converting to numeric"


library(RColorBrewer)


####Experimenting with cubic splines

library(splines)
library(MASS)

#spline example
#p <- ggplot(objsbj.data, aes(Year, OV, color=Clause, group=Clause)) + labs(y = "Proportion of OV", x = "\nYear") + stat_sum(aes(size=..n.., alpha=.5)) + scale_size_area(max_size=12) + stat_smooth(method= "lm", formula = y ~ ns(x,3)) + scale_alpha_continuous(guide="none", limits = c(0,.7)) + scale_color_brewer(palette = "Set1") + ylim(0,1)



##Gotta do invq separately, without having a third variable, because there isn't enough data

objsbjNOinvq.data <- subset(objsbj.data, Clause != "invq")

objsbjNOinvq.data <- droplevels(objsbjNOinvq.data)

p <- ggplot(objsbjNOinvq.data, aes(Year, OV, color=SbjType)) + 
  labs(y = "Proportion of OV", x = "\nYear") + 
  stat_sum(aes(size=..n.., alpha=.1)) + 
  scale_size_area(max_size=12) + 
  stat_smooth() + 
  #aes(linetype=ObjType) +
  #scale_linetype_manual(values=c("solid","dotted","dashed")) + 
  scale_alpha_continuous(guide="none", limits = c(0,.7)) + 
  scale_color_brewer(palette = "Set1") + 
  ylim(0,1) + 
  facet_grid(ObjType~Clause) + #facet_wrap(~ObjType) +
  theme_bw() + theme(panel.border = element_blank())

ggsave(p, file = "infoTheory-objsbjmatsub-English.pdf", width = 8, height = 5)

##simpler plot with obj type restricted to nominal, and removing gap subjects, just for clarity of explication in a CBE talk

objsbjNOinvq.data <- subset(objsbjNOinvq.data, ObjType == "posobj")

objsbjNOinvq.data <- droplevels(objsbjNOinvq.data)

objsbjNOinvq.data <- subset(objsbjNOinvq.data, SbjType != "gapsbj")

objsbjNOinvq.data <- droplevels(objsbjNOinvq.data)

##simpler plot with clause type restricted to sub, and no qobjs, just for clarity of explication in oxford thing

objsbjNOinvq.data <- subset(objsbjNOinvq.data, Clause == "sub")

objsbjNOinvq.data <- droplevels(objsbjNOinvq.data)

objsbjNOinvq.data <- subset(objsbjNOinvq.data, ObjType != "qobj")

objsbjNOinvq.data <- droplevels(objsbjNOinvq.data)



p <- ggplot(objsbjNOinvq.data, aes(Year, OV, color=SbjType)) + 
  labs(y = "Proportion of OV", x = "\nYear") + 
  stat_sum(aes(size=..n.., alpha=.1)) + 
  scale_size_area(max_size=12) + 
  stat_smooth() + 
  #aes(linetype=ObjType) +
  #scale_linetype_manual(values=c("solid","dotted","dashed")) + 
  scale_alpha_continuous(guide="none", limits = c(0,.7)) + 
  scale_color_brewer(palette = "Set1") + 
  ylim(0,1) + 
  facet_grid(~Clause) +
  theme_bw() + theme(panel.border = element_blank())

ggsave(p, file = "infoTheory-posObjsbjmatsub-English.pdf", width = 8, height = 5)


####little tests
library(lme4)
#Zing the year around the mean year
objsbjNOinvq.data$zYear <- scale(objsbjNOinvq.data$Year, center=TRUE, scale=TRUE)

#Make constrasts consistent
objsbjNOinvq.data$ObjTypeRelevel <- relevel(objsbjNOinvq.data$ObjType, ref="posobj")
objsbjNOinvq.data$SbjTypeRelevel <- relevel(objsbjNOinvq.data$SbjType, ref="nomsbj")

sbjobj.fit <- glmer(OV~(1|ID)+Clause+zYear+SbjTypeRelevel+ObjTypeRelevel+SbjTypeRelevel:zYear, family = binomial, data=objsbjNOinvq.data)
summary(sbjobj.fit)
sbjobjNoYear.fit <- glmer(OV~(1|ID)+Clause+zYear+SbjTypeRelevel+ObjTypeRelevel, family = binomial, data=objsbjNOinvq.data)
sbj.obj.fit <- glmer(OV~(1|ID)+Clause+zYear+SbjTypeRelevel+ObjTypeRelevel, family = binomial, data=objsbjNOinvq.data)
basic.fit <- glmer(OV~(1|ID)+Clause+zYear, family = binomial, data=objsbjNOinvq.data)

summary(sbjobjNoYear.fit)
anova(sbj.obj.fit,basic.fit)

#For Ice
objsbjNOinvqIce.data$ObjTypeRelevel <- relevel(objsbjNOinvqIce.data$ObjType, ref="pronobj")
objsbjNOinvqIce.data$SbjTypeRelevel <- relevel(objsbjNOinvqIce.data$SbjType, ref="pronsbj")
objsbjNOinvqIce.data$zYear <- scale(objsbjNOinvqIce.data$Year, center=TRUE, scale=TRUE)

sbj.obj.fit <- glmer(OV~(1|ID)+Clause+SimpleGenre+zYear+SbjTypeRelevel+ObjTypeRelevel, family = binomial, data=objsbjNOinvqIce.data)
basic.fit <- glmer(OV~(1|ID)+SimpleGenre+Clause+zYear, family = binomial, data=objsbjNOinvqIce.data)

summary(sbjobjNoYear.fit)
anova(sbj.obj.fit,basic.fit)



#More complex ones are not converging, so standard regressions:
sbjobjYear.fit <- glm(OV~Clause+zYear+SbjTypeRelevel+ObjTypeRelevel+zYear*SbjTypeRelevel*ObjTypeRelevel, family = binomial, data=objsbjNOinvq.data)
summary(sbjobjYear.fit)
sbjobjNoYear.fit <- glm(OV~Clause+zYear+SbjTypeRelevel+ObjTypeRelevel+SbjTypeRelevel*ObjTypeRelevel, family = binomial, data=objsbjNOinvq.data)
summary(sbjobjNoYear.fit)
anova(sbjobjNoYear.fit, sbjobjYear.fit, test="Chisq")

#For Ice
sbjobjYear.fit <- glm(OV~Clause+SimpleGenre+zYear+SbjTypeRelevel+ObjTypeRelevel+zYear*SbjTypeRelevel*ObjTypeRelevel, family = binomial, data=objsbjNOinvqIce.data)
summary(sbjobjYear.fit)
sbjobjNoYear.fit <- glm(OV~Clause+SimpleGenre+zYear+SbjTypeRelevel+ObjTypeRelevel+SbjTypeRelevel*ObjTypeRelevel, family = binomial, data=objsbjNOinvqIce.data)
summary(sbjobjNoYear.fit)
anova(sbjobjNoYear.fit, sbjobjYear.fit, test="Chisq")




##invq data separately:

invq.data <- subset(objsbj.data, Clause == "invq")

invq.data <- droplevels(invq.data)

p <- ggplot(invq.data, aes(Year, OV, color=SbjType)) + labs(y = "Proportion of OV", x = "\nYear") + stat_sum(aes(size=..n.., alpha=.5)) + scale_size_area(max_size=12) + stat_smooth() + scale_alpha_continuous(guide="none", limits = c(0,.7)) + scale_color_brewer(palette = "Set1") + ylim(0,1)



####Icelandic


####Read the file of CorpusSearch codes into an R data frame.

foo <- read.delim("~/constantentropy/outputs/infoTheoryTest.ice.cod.ooo",header=F,sep=":")


####Give appropriate column names to the columns

colnames(foo) <- c("OV","Clause","ObjType","SbjType","Year","Genre","ID")


####Throw out all the codes that refer to tokens that are irrelevant for the study.

"Got up to subsetting"

objsbjice.data <- subset(foo, OV != "z" & Clause != "z" & Year != "0" & Year != "" & Year != "na" & SbjType != "z" & SbjType != "" & ObjType != "z" & ObjType != "" & ID != "" & Genre != "")


####Make sure R factor groups don't include factors for the irrelevant codes.

objsbjice.data <- droplevels(objsbjice.data)

"finished droplevels"

####Make sure dates abd 0/1 codes are stored as numbers, and weights

objsbjice.data$Year <- as.numeric(as.character(objsbjice.data$Year))
objsbjice.data$OV <- as.numeric(as.character(objsbjice.data$OV))

"finished converting to numeric"


##Gotta do invq separately, without having a third variable, because there isn't enough data



objsbjNOinvqIce.data$SimpleGenre <- ifelse(objsbjNOinvqIce.data$Genre == "nar", "nar", "other")

objsbjNOinvqIce.data <- subset(objsbjice.data, Clause != "invq")

objsbjNOinvqIce.data <- droplevels(objsbjNOinvqIce.data)

p <- ggplot(objsbjNOinvqIce.data, aes(Year, OV, color=SbjType)) + 
  labs(y = "Proportion of OV", x = "\nYear") + 
  stat_sum(aes(size=..n.., alpha=.1)) + 
  scale_size_area(max_size=12) + 
  stat_smooth() + 
  scale_alpha_continuous(guide="none", limits = c(0,.7)) + 
  scale_color_brewer(palette = "Set1") + 
  ylim(0,1) + 
  facet_grid(ObjType~Clause) +
  theme_bw() + theme(panel.border = element_blank())

ggsave(p, file = "infoTheory-objsbjmatsub-Ice.pdf", width = 8, height = 5)

###same for narrative texts only

objsbjNOinvqIceNar.data <- subset(objsbjNOinvqIce.data, Genre == "nar")

objsbjNOinvqIceNar.data <- droplevels(objsbjNOinvqIceNar.data)

p <- ggplot(objsbjNOinvqIceNar.data, aes(Year, OV, color=SbjType)) + 
  labs(y = "Proportion of OV", x = "\nYear") + 
  stat_sum(aes(size=..n.., alpha=.1)) + 
  scale_size_area(max_size=12) + 
  stat_smooth() + 
  scale_alpha_continuous(guide="none", limits = c(0,.7)) + 
  scale_color_brewer(palette = "Set1") + 
  ylim(0,1) + 
  facet_grid(ObjType~Clause) + # facet_wrap(~ObjType) +
  theme_bw() + theme(panel.border = element_blank())

ggsave(p, file = "infoTheory-objsbjmatsubNar-Ice.pdf", width = 8, height = 5)


##simpler plot with obj type restricted to nominal, and removing gap subjects, just for clarity of explication in a CBE talk

objsbjNOinvqIceNar.data <- subset(objsbjNOinvqIceNar.data, ObjType == "posobj")

objsbjNOinvqIceNar.data <- droplevels(objsbjNOinvqIceNar.data)

objsbjNOinvqIceNar.data <- subset(objsbjNOinvqIceNar.data, SbjType != "gapsbj")

objsbjNOinvqIceNar.data <- droplevels(objsbjNOinvqIceNar.data)

##simpler plot with clause type restricted to sub, and no qobjs, just for clarity of explication in oxford thing

objsbjNOinvqIceNar.data <- subset(objsbjNOinvqIceNar.data, Clause == "sub")

objsbjNOinvqIceNar.data <- droplevels(objsbjNOinvqIceNar.data)

objsbjNOinvqIceNar.data <- subset(objsbjNOinvqIceNar.data, ObjType != "qobj")

objsbjNOinvqIceNar.data <- droplevels(objsbjNOinvqIceNar.data)



p <- ggplot(objsbjNOinvqIceNar.data, aes(Year, OV, color=SbjType)) + 
  labs(y = "Proportion of OV", x = "\nYear") + 
  stat_sum(aes(size=..n.., alpha=.1)) + 
  scale_size_area(max_size=12) + 
  stat_smooth() + 
  #aes(linetype=ObjType) +
  #scale_linetype_manual(values=c("solid","dotted","dashed")) + 
  scale_alpha_continuous(guide="none", limits = c(0,.7)) + 
  scale_color_brewer(palette = "Set1") + 
  ylim(0,1) + 
  facet_grid(~Clause) +
  theme_bw() + theme(panel.border = element_blank())

ggsave(p, file = "infoTheory-posObjsbjmatsub-Ice.pdf", width = 8, height = 5)

###same for religious texts only

objsbjNOinvqIceRel.data <- subset(objsbjNOinvqIce.data, Genre == "rel")

objsbjNOinvqIceRel.data <- droplevels(objsbjNOinvqIceRel.data)

p <- ggplot(objsbjNOinvqIceRel.data, aes(Year, OV, color=SbjType)) + 
  labs(y = "Proportion of OV", x = "\nYear") + 
  stat_sum(aes(size=..n.., alpha=.1)) + 
  scale_size_area(max_size=12) + 
  stat_smooth() + 
  scale_alpha_continuous(guide="none", limits = c(0,.7)) + 
  scale_color_brewer(palette = "Set1") + 
  ylim(0,1) + 
  facet_grid(ObjType~Clause) +
  theme_bw() + theme(panel.border = element_blank())

ggsave(p, file = "infoTheory.objsbjmatsubRel.Ice.pdf", width = 8, height = 5)
