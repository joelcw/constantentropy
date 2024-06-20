library(ggplot2)
library(plyr)
library(gdata)
library(lme4)

####Read the file of CorpusSearch codes into an R data frame.

foo <- read.delim("~/constantentropy/outputs/ipmat-obj-sbj-vfin.eb.cod.ooo",header=F,sep=":")


####Give appropriate column names to the columns

colnames(foo) <- c("Top","ObjType","SbjType","ID","Text","Year","CalDorm")

####Throw out all the codes that refer to tokens that are irrelevant for the study. Also throw out Nwe Test translation because it's an outlier, 

"Got up to subsetting"

top.data <- subset(foo, Top != "z" & ObjType != "z" & SbjType != "z" & Year != "0" & Year != "" & Year != "na" & Text != "" & Text != "z")

####Make sure R factor groups don't include factors for the irrelevant codes.

top.data <- droplevels(top.data)

"finished droplevels"


####Make sure dates abd 0/1 codes are stored as numbers, and weights

top.data$Year <- as.numeric(as.character(top.data$Year))
top.data$Top <- as.numeric(as.character(top.data$Top))


"finished converting to numeric"



p <- ggplot(top.data, aes(Year, Top, color=ObjType, group=ObjType)) + 
  labs(y = "Proportion of Obj Fronting", x = "\nYear") + 
  stat_sum(aes(size=..n.., alpha=.1)) + 
  scale_size_area(max_size=12) + 
  facet_wrap(~SbjType) +
  #stat_smooth(method="glm", family ="binomial")
  stat_smooth() + 
  scale_alpha_continuous(guide="none", limits = c(0,.7)) + 
  scale_color_brewer(palette = "Set1") + 
  ylim(0,1) + 
  theme_bw() + theme(panel.border = element_blank())

ggsave(p, file = "", width = 8, height = 5)



ggplot(nondemsbj.df, aes(Year, Top, color=ObjType, group=ObjType)) + 
  labs(y = "Proportion of Obj Fronting", x = "\nYear") + 
  stat_sum(aes(size=..n.., alpha=.1)) + 
  scale_size_area(max_size=12) + 
  facet_wrap(~SbjType) +
  #stat_smooth(method="glm", family ="binomial")
  stat_smooth() + 
  scale_alpha_continuous(guide="none", limits = c(0,.7)) + 
  scale_color_brewer(palette = "Set1") + 
  ylim(0,1) + 
  theme_bw() + theme(panel.border = element_blank())


#Looking at non-dem objs only:
top.data$SbjType <- as.factor(top.data$SbjType)
top.data$ObjType <- as.factor(top.data$ObjType)
nondem.df <- subset(top.data,ObjType != "demobj")
nondem.df <- droplevels(nondem.df)

ggplot(nondem.df, aes(Year, Top, color=ObjType, group=ObjType)) + 
  labs(y = "Proportion of Obj Fronting", x = "\nYear") + 
  stat_sum(aes(size=..n.., alpha=.1)) + 
  scale_size_area(max_size=12) + 
  facet_wrap(~SbjType) +
  #stat_smooth(method="glm", family ="binomial")
  stat_smooth() + 
  scale_alpha_continuous(guide="none", limits = c(0,.7)) + 
  scale_color_brewer(palette = "Set1") + 
  ylim(0,1) + 
  theme_bw() + theme(panel.border = element_blank())

#binning by 50 years and converting to proportion so we can see anything at all:
nondem.df$Year2 <- floor(nondem.df$Year/50)*50
plot.data <- ddply(nondem.df, .(Year2, SbjType, ObjType),summarize, fronted = mean(Top, na.rm = T), n = sum(!is.na(Top)))

p <- ggplot(plot.data, aes(Year2, fronted, color=SbjType, group=SbjType)) + 
  labs(y = "Proportion of Obj Fronting", x = "\nYear") + 
#  scale_alpha_continuous(guide="none", limits = c(0,0.1)) +
  scale_size_area(max_size=5) +
  geom_point(aes(size = n)) +
  facet_wrap(~ObjType) +
  #stat_smooth(method="glm", family ="binomial")
  stat_smooth() + 
  scale_color_brewer(palette = "Set1") + 
  ylim(0,0.07) + 
  theme_bw() + theme(panel.border = element_blank())

ggsave(p, file = "~/constantentropy/ittalks/objTop.png", width = 8, height = 5)
####NOTE: the dmsbj nomobj fronted is only 1 out of 25 token in 1500 and 1 out of 70 in 1650

library(lme4)

#Looking at non-dem sbjs:
nondemsbj.df <- subset(nondem.df,SbjType != "demsbj")
nondemsbj.df <- droplevels(nondemsbj.df)

nondemsbj.df$zYear <- scale(nondemsbj.df$Year, center=TRUE, scale=TRUE)
#3-way didn't converge:
basic.fit <- glmer(Top~(1|Text)+zYear+SbjType+ObjType+SbjType*zYear+ObjType*zYear, family = binomial, data=nondemsbj.df)
summary(basic.fit)

#Non-hierarchical, because 2-way and 3-ways weren't converging with mixed:
no3way.fit <- glm(Top~zYear+SbjType+ObjType+SbjType*ObjType*zYear, family = binomial, data=nondemsbj.df)
summary(no3way.fit)

#Again, but only after 1700
nondemsbj1742.df <- nondemsbj.df[nondemsbj.df$Year > 1742,]
nondemsbj1742.df$zYear <- scale(nondemsbj1742.df$Year, center=TRUE, scale=TRUE)

yearInter1742.fit <- glmer(Top~(1|Text)+zYear+SbjType*ObjType+SbjType*zYear+ObjType*zYear, family = binomial, data=nondemsbj1742.df)
summary(yearInter1742.fit)

noYear1742.fit <- glmer(Top~(1|Text)+SbjType*ObjType, family = binomial, data=nondemsbj1742.df)
anova(yearInter1742.fit, noYear1742.fit,test="Chisq")

#basic1742.fit <- glmer(Top~(1|Text)+zYear+SbjType+ObjType, family = binomial, data=nondemsbj1742.df)
#summary(basic1742.fit)
anova(basic1742.fit, noYear1742.fit,test="Chisq")



ggplot(plot.data[plot.data$Year2 > 1742,], aes(Year2, fronted, color=SbjType, group=SbjType)) + 
  labs(y = "Proportion of Obj Fronting", x = "\nYear") + 
  #  scale_alpha_continuous(guide="none", limits = c(0,0.1)) +
  scale_size_area(max_size=5) +
  geom_point(aes(size = n)) +
  facet_wrap(~ObjType) +
  #stat_smooth(method="glm", family ="binomial")
  stat_smooth() + 
  scale_color_brewer(palette = "Set1") + 
  ylim(0,0.07) + 
  theme_bw() + theme(panel.border = element_blank())

####Speyer's data

topCounts.df <- data.frame(expand.grid(Order=order,Period=period,Count = 0))