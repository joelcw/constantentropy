library(ggplot2)
library(plyr)

####For English data.
####Read the file of CorpusSearch codes into an R data frame.

foo <- read.delim("outputs/infoTheoryTest.ymeb.cod.ooo",header=F,sep=":")


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
  facet_grid(ObjType~Clause)

ggsave(p, file = "infoTheory.objsbjmatsub.English.pdf", width = 8, height = 5)


##invq data separately:

invq.data <- subset(objsbj.data, Clause == "invq")

invq.data <- droplevels(invq.data)

p <- ggplot(invq.data, aes(Year, OV, color=SbjType)) + labs(y = "Proportion of OV", x = "\nYear") + stat_sum(aes(size=..n.., alpha=.5)) + scale_size_area(max_size=12) + stat_smooth() + scale_alpha_continuous(guide="none", limits = c(0,.7)) + scale_color_brewer(palette = "Set1") + ylim(0,1)

