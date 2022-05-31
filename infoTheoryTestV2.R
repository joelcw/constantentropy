library(ggplot2)
library(plyr)
library(gdata)

####Icelandic


####Read the file of CorpusSearch codes into an R data frame.

foo <- read.delim("~/constantentropy/outputs/infoTheoryTestV2.ice.IDfixed.cod.ooo",header=F,sep=":")
pps <- read.delim("~/constantentropy/outputs/infoTheoryTestV2PPs.ice.cod.ooo",header=F,sep=":") #subset of same data: adjuncts only PPs



####Give appropriate column names to the columns

colnames(foo) <- c("OV","V2","ObjType","SbjType","Year","Genre","ID","TreeID")
colnames(pps) <- c("OV","V2","ObjType","SbjType","Year","Genre","ID")

####Throw out all the codes that refer to tokens that are irrelevant for the study. Also throw out Nwe Test translation because it's an outlier, 

"Got up to subsetting"

objsbjice.data <- subset(foo, OV != "z" & V2 != "z" & ObjType != "z" & SbjType != "z" & Year != "0" & Year != "" & Year != "na" & ID != "" & ID != "1540.ntacts.rel-bib" & ID != "1540.ntjohn.rel-bib" & Genre != "")
objsbjicePPs.data  <- subset(pps, OV != "z" & V2 != "z" & ObjType != "z" & SbjType != "z" & Year != "0" & Year != "" & Year != "na" & ID != "" & ID != "1540.ntacts.rel-bib" & ID != "1540.ntjohn.rel-bib" & Genre != "")

####Make sure R factor groups don't include factors for the irrelevant codes.

objsbjice.data <- droplevels(objsbjice.data)
objsbjicePPs.data <- droplevels(objsbjicePPs.data)

"finished droplevels"

###Trying to get DORMS by merging with old output file that has the DORMS
dormOutput <- read.delim("~/iceBits/ovCodingTreeAndClauseDormuido.tsv",header=T,sep="\t")
dorm.df <- data.frame(dormOutput$TextId,dormOutput$TreeId,dormOutput$SentDormUido)
colnames(dorm.df) <- c("ID","TreeID","SentDormUido")
fulldata <- merge(objsbjice.data, dormOutput, by=c("ID","TreeID"), all.x=TRUE, all.y=FALSE)


####Make sure dates abd 0/1 codes are stored as numbers, and weights

objsbjice.data$Year <- as.numeric(as.character(objsbjice.data$Year))
objsbjice.data$OV <- as.numeric(as.character(objsbjice.data$OV))
objsbjicePPs.data$Year <- as.numeric(as.character(objsbjicePPs.data$Year))
objsbjicePPs.data$OV <- as.numeric(as.character(objsbjicePPs.data$OV))

"finished converting to numeric"



objsbjice.data$SimpleGenre <- ifelse(objsbjice.data$Genre == "nar", "nar", "other")
objsbjicePPs.data$SimpleGenre <- ifelse(objsbjicePPs.data$Genre == "nar", "nar", "other")


p <- ggplot(objsbjice.data, aes(Year, OV, color=V2)) + 
  labs(y = "Proportion of OV", x = "\nYear") + 
  stat_sum(aes(size=..n.., alpha=.1)) + 
  scale_size_area(max_size=12) + 
  facet_wrap(~SimpleGenre) +
  #stat_smooth(method="glm", family ="binomial")
  stat_smooth() + 
  scale_alpha_continuous(guide="none", limits = c(0,.7)) + 
  scale_color_brewer(palette = "Set1") + 
  ylim(0,1) + 
  facet_grid(ObjType~SimpleGenre) +
  theme_bw() + theme(panel.border = element_blank())

ggsave(p, file = "infoTheoryV2.pdf", width = 8, height = 5)

###same for narrative texts only

objsbjIceNar.data <- subset(objsbjice.data, Genre == "nar")
objsbjIceNar.data <- droplevels(objsbjIceNar.data)

objsbjIceNarPPs.data <- subset(objsbjicePPs.data, Genre == "nar")
objsbjIceNarPPs.data <- droplevels(objsbjIceNarPPs.data)

objsbjIceNar.data$OV <- as.numeric(as.character(objsbjIceNar.data$OV))


p <- ggplot(objsbjicer.data, aes(Year, OV, color=V2)) + 
  labs(y = "Proportion of OV", x = "\nYear") + 
  stat_sum(aes(size=..n.., alpha=.1)) + 
  scale_size_area(max_size=12) + 
  stat_smooth() + 
  scale_alpha_continuous(guide="none", limits = c(0,.7)) + 
  scale_color_brewer(palette = "Set1") + 
  ylim(0,1) + 
  facet_grid(ObjType~SbjType) +
  theme_bw() + theme(panel.border = element_blank())

ggsave(p, file = "V2OVNar.png", width = 8, height = 5)

#Simplifying further by leaving out quantified objects and gapped subjects:
objsbjIceNar.data <- subset(objsbjIceNar.data, ObjType != "qobj")
objsbjIceNar.data <- subset(objsbjIceNar.data, SbjType != "gapsbj")
objsbjIceNar.data <- droplevels(objsbjIceNar.data)

objsbjIceNarPPs.data <- subset(objsbjIceNarPPs.data, ObjType != "qobj")
objsbjIceNarPPs.data <- subset(objsbjIceNarPPs.data, SbjType != "gapsbj")
objsbjIceNarPPs.data <- droplevels(objsbjIceNarPPs.data)

#Relabelling for plot beauty:
objsbjIceNarPlot.data <-objsbjIceNar.data
objsbjIceNar.data$V2 <- ifelse(objsbjIceNar.data$V2 == "sinv","Fronting","No Fronting")
objsbjIceNarPlot.data$SbjType <- ifelse(objsbjIceNarPlot.data$SbjType == "nomsbj","Nominal Sbj","Pron Sbj")
objsbjIceNarPlot.data$ObjType <- ifelse(objsbjIceNarPlot.data$ObjType == "posobj","Nominal Obj","Pron Obj")

q <- ggplot(objsbjIceNarPlot.data, aes(Year, OV, color=V2)) + 
  labs(y = "Proportion of V2", x = "\nYear") + 
  stat_sum(aes(size=..n.., alpha=.1)) + 
  scale_size_area(max_size=12) + 
  stat_smooth() + 
  scale_alpha_continuous(guide="none", limits = c(0,.7)) + 
  scale_color_brewer(palette = "Set1") + 
  ylim(0,1) + 
  facet_grid(ObjType~SbjType) +
  theme_bw() + theme(panel.border = element_blank())

ggsave(q, file = "~/constantentropy/ittalks/V2objsbjOVNar.png", width = 8, height = 5)

library(splines)
library(MASS)
#With cubic splines instead:
q <- ggplot(objsbjIceNar.data, aes(Year, OV, color=V2)) + 
  labs(y = "Proportion of OV", x = "\nYear") + 
  stat_sum(aes(size=..n.., alpha=.1)) + 
  scale_size_area(max_size=12) + 
  stat_smooth(method= "lm", formula = y ~ ns(x,4)) +
  #stat_smooth(method="glm", family ="binomial",fullrange=F) +
  scale_alpha_continuous(guide="none", limits = c(0,.7)) + 
  scale_color_brewer(palette = "Set1") + 
  ylim(0,1) + 
  facet_grid(ObjType~SbjType) +
  theme_bw() + theme(panel.border = element_blank())


###Making a plot with V2 as outcome:
objsbjIceNar.data$V2bin <- ifelse(objsbjIceNar.data$V2 == "sinv", 1, 0)
objsbjIceNar.data$OV <- as.factor(objsbjIceNar.data$OV)

objsbjIceNarPPs.data$V2bin <- ifelse(objsbjIceNarPPs.data$V2 == "sinv", 1, 0)
objsbjIceNarPPs.data$OV <- as.factor(objsbjIceNarPPs.data$OV)


plot.data <- ddply(objsbjIceNar.data, .(ObjType,SbjType,OV),summarize, Inversion = mean(V2bin, na.rm = T), n = sum(!is.na(V2bin)))
plot.data$OV <- ifelse(plot.data$OV == 1, "OV", "VO")
plot.data$ObjType <- ifelse(plot.data$ObjType == "posobj", "Nominal Obj", "Pronoun Obj")
plot.data$SbjType <- ifelse(plot.data$SbjType == "nomsbj", "Nominal Sbj", "Pronoun Sbj")

write.csv(plot.data,file="~/constantentropy/outputs/V2plot.data")
write.csv(objsbjIceNar.data,file="~/constantentropy/outputs/objsbjIceNar.data")

q <- ggplot(plot.data, aes(SbjType,Inversion)) + 
  scale_y_continuous(name = "Proportion Inversion (V2)", limits = c(0,1) ) + 
  scale_x_discrete(name = "\nOrder") + 
  geom_bar(fill=c("black","gray","black","gray","black","gray","black","gray"), stat="identity") +
  facet_grid(ObjType~OV) +
  theme_bw() + 
  theme(panel.border = element_blank())

ggsave(q, file = "constantentropy/ittalks/V2bars.png", width = 8, height = 5)


###Note that the preliminary test from the abstract only included nominal sbj and pos objs. I have re-done it below adding in prons, but not qs or gapsbjs
###NOTE THAT GAPSBJS GET CODED AS NON-INVERTED! Also, impressionistically, when the advP is not fronted, it tends to be placed finally in OV clauses if possible.
library(lme4)

objsbjIceNar.data <- subset(objsbjIceNar.data, ObjType != "qobj")
objsbjIceNar.data <- subset(objsbjIceNar.data, SbjType != "gapsbj")
objsbjIceNar.data <- droplevels(objsbjIceNar.data)

objsbjIceNar.data$OV <- as.numeric(as.character(objsbjIceNar.data$OV))

objsbjIceNar.data$zYear <- scale(objsbjIceNar.data$Year, center=TRUE, scale=TRUE)
objsbjIceNarPPs.data$zYear <- scale(objsbjIceNarPPs.data$Year, center=TRUE, scale=TRUE)

basic.fit <- glm(OV~zYear+V2*SbjType*ObjType+V2:zYear+SbjType:zYear+ObjType:zYear, family = binomial, data=objsbjIceNar.data)
#basic.fit <- glm(OV~zYear+V2+V2:zYear, family = binomial, data=objsbjIceNar.data)
summary(basic.fit)

objsbjice.data <- subset(objsbjice.data, ObjType != "qobj")
objsbjice.data <- subset(objsbjice.data, SbjType != "gapsbj")

objsbjicePPs.data <- subset(objsbjicePPs.data, ObjType != "qobj")
objsbjicePPs.data <- subset(objsbjicePPs.data, SbjType != "gapsbj")


objsbjice.data <- droplevels(objsbjice.data)
objsbjice.data$zYear <- scale(objsbjice.data$Year, center=TRUE, scale=TRUE)
objsbjicePPs.data$zYear <- scale(objsbjicePPs.data$Year, center=TRUE, scale=TRUE)

basicGenre.fit <- glm(OV~zYear+V2*SbjType*ObjType+V2:zYear+SimpleGenre+SimpleGenre:zYear+SbjType:zYear+ObjType:zYear, family = binomial, data=objsbjice.data)
summary(basicGenre.fit)




#Modelling V2 as outcome:
####NOTE THAT OUR REAL PREDICTION, WHICH YOU CAN'T YET SEE IN THIS DATA, IS THAT YOU FRONT/V2 WHEN IT RESULTS IN A LOWER DORM
#The other prediction, is that if V2 is a choice made after ov/vo, then ov/vo wont predict dorm for thid data set, but v2 will
objsbjIceNar.data$V2bin <- as.numeric(as.character(objsbjIceNar.data$V2bin))
V2.fit <- glm(V2bin~zYear+OV*SbjType*ObjType+OV:zYear, family = binomial, data=objsbjIceNar.data)
summary(V2.fit)

objsbjice.data$V2bin <- ifelse(objsbjice.data$V2 == "sinv", 1, 0)
V2.fit2 <- glm(V2bin~zYear+OV*SbjType*ObjType+OV:zYear+SimpleGenre*OV, family = binomial, data=objsbjice.data)
summary(V2.fit2)


#Relevel
objsbjIceNar.data$ObjTypeRelevel <- relevel(objsbjIceNar.data$ObjType, ref="pronobj")
objsbjIceNar.data$SbjTypeRelevel <- relevel(objsbjIceNar.data$SbjType, ref="pronsbj")
V2.fit <- glm(V2bin~zYear+OV*SbjTypeRelevel*ObjTypeRelevel+OV:zYear, family = binomial, data=objsbjIceNar.data)
summary(V2.fit)

V2.OVobj.OVsbj.fit <- glm(V2bin~zYear+OV*SbjTypeRelevel+OV*ObjTypeRelevel+OV:zYear, family = binomial, data=objsbjIceNar.data)
anova(V2.OVobj.OVsbj.fit,V2.fit,test="Chisq")

#Same, relevelled, with PPs
objsbjIceNarPPs.data$ObjTypeRelevel <- relevel(objsbjIceNarPPs.data$ObjType, ref="pronobj")
objsbjIceNarPPs.data$SbjTypeRelevel <- relevel(objsbjIceNarPPs.data$SbjType, ref="pronsbj")
#This ends up nonsignificant with p-values, but with massive effect sizes...so I think the N is just too low.
V2PPs.fit <- glm(V2bin~zYear+OV*SbjType*ObjTypeRelevel+OV:zYear, family = binomial, data=objsbjIceNarPPs.data)
summary(V2PPs.fit)
nrow(objsbjIceNarPPs.data)

#If you do it with the larger data set, including other genres, you get the same effects as in the non-PP one:
objsbjicePPs.data$V2bin <- ifelse(objsbjicePPs.data$V2 == "sinv", 1, 0)
V2.fit2 <- glm(V2bin~zYear+OV*SbjType*ObjType+OV:zYear+SimpleGenre*OV, family = binomial, data=objsbjicePPs.data)
summary(V2.fit2)


###Mixed effects, testing CRE
library(lme4)
objsbjice.data$zYear <- scale(objsbjice.data$Year, center=TRUE, scale=TRUE)

#CONVERGING: at least shows that V2 doesn't affect OV in a vacuum
noInter.fit <- glmer(OV~(1|ID)+V2+SbjType+ObjType+zYear+SimpleGenre, family = binomial, objsbjice.data)
summary(noInter.fit)
twoWayInters.fit <- glmer(OV~(1|ID)+SbjType*V2+ObjType*V2+V2*zYear+SimpleGenre:zYear, family = binomial, objsbjice.data)
summary(twoWayInters.fit)

#NOT CONVERGING
someInter.fit <- glmer(OV~(1|ID)+V2+SbjType*ObjType+zYear+SimpleGenre, family = binomial, objsbjice.data)
someInter.fit <- glmer(OV~(1|ID)+V2*SbjType+ObjType+zYear+SimpleGenre, family = binomial, objsbjice.data)
someInter.fit <- glmer(OV~(1|ID)+SbjType+V2*ObjType+zYear+SimpleGenre, family = binomial, objsbjice.data)
twoWayInters.fit <- glmer(OV~(1|ID)+SbjType*V2+ObjType*V2+V2*zYear+SimpleGenre:zYear, family = binomial, objsbjice.data)


basicInter.fit <- glm(OV~V2*zYear*SbjType*ObjType*SimpleGenre, family = binomial, data=objsbjice.data)
summary(basicInter.fit)


