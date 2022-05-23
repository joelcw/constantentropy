library(ggplot2)
library(plyr)

####Icelandic


####Read the file of CorpusSearch codes into an R data frame.

foo <- read.delim("~/constantentropy/outputs/infoTheoryTestV2.ice.IDfixed.cod.ooo",header=F,sep=":")


####Give appropriate column names to the columns

colnames(foo) <- c("OV","V2","ObjType","SbjType","Year","Genre","ID")


####Throw out all the codes that refer to tokens that are irrelevant for the study. Also throw out Nwe Test translation because it's an outlier, 

"Got up to subsetting"

objsbjice.data <- subset(foo, OV != "z" & V2 != "z" & ObjType != "z" & SbjType != "z" & Year != "0" & Year != "" & Year != "na" & ID != "" & ID != "1540.ntacts.rel-bib" & ID != "1540.ntjohn.rel-bib" & Genre != "")


####Make sure R factor groups don't include factors for the irrelevant codes.

objsbjice.data <- droplevels(objsbjice.data)

"finished droplevels"

####Make sure dates abd 0/1 codes are stored as numbers, and weights

objsbjice.data$Year <- as.numeric(as.character(objsbjice.data$Year))
objsbjice.data$OV <- as.numeric(as.character(objsbjice.data$OV))

"finished converting to numeric"



objsbjice.data$SimpleGenre <- ifelse(objsbjice.data$Genre == "nar", "nar", "other")


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

p <- ggplot(objsbjIceNar.data, aes(Year, OV, color=V2)) + 
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
objsbjIceNar.data <- subset(objsbjIceNar.data, ObjType == "posobj")
objsbjIceNar.data <- subset(objsbjIceNar.data, SbjType != "gapsbj")
objsbjIceNar.data <- droplevels(objsbjIceNar.data)

ggplot(objsbjIceNar.data, aes(Year, OV, color=V2)) + 
  labs(y = "Proportion of OV", x = "\nYear") + 
  stat_sum(aes(size=..n.., alpha=.1)) + 
  scale_size_area(max_size=12) + 
  stat_smooth() + 
  scale_alpha_continuous(guide="none", limits = c(0,.7)) + 
  scale_color_brewer(palette = "Set1") + 
  ylim(0,1) + 
  facet_grid(ObjType~SbjType) +
  theme_bw() + theme(panel.border = element_blank())




###Little test
library(lme4)
objsbjIceNar.data$zYear <- scale(objsbjIceNar.data$Year, center=TRUE, scale=TRUE)
basic.fit <- glm(OV~V2+zYear+V2:zYear, family = binomial, data=objsbjIceNar.data)
objsbjice.data$zYear <- scale(objsbjice.data$Year, center=TRUE, scale=TRUE)
basic.fit <- glm(OV~V2+zYear+V2:zYear, family = binomial, data=objsbjice.data)
summary(basic.fit)




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
