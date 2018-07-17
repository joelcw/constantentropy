library(ggplot2)
library(plyr)

####For English data.
####Read the file of CorpusSearch codes into an R data frame.

foo <- read.delim("infoTheoryTest.cod.ooo",header=F,sep=":")


####Give appropriate column names to the columns

colnames(foo) <- c("OV","Clause","Year","sbj")


####Throw out all the codes that refer to tokens that are irrelevant for the study.

"Got up to subsetting"

ex.data <- subset(foo, OV != "z" & Clause != "z" & Year != "0" & Year != "" & Year != "NA" & sbj != "z" & sbj != "")


library(gdata)


####Make sure R factor groups don't include factors for the irrelevant codes.

ex.data <- droplevels(ex.data)

"finished droplevels"

####Make sure dates abd 0/1 codes are stored as numbers, and weights

ex.data$Year <- as.numeric(as.character(ex.data$Year))
ex.data$OV <- as.numeric(as.character(ex.data$OV))

"finished converting to numeric"


library(RColorBrewer)


####Experimenting with cubic splines

library(splines)
library(MASS)

#spline example
#p <- ggplot(ex.data, aes(Year, OV, color=Clause, group=Clause)) + labs(y = "Proportion of OV", x = "\nYear") + stat_sum(aes(size=..n.., alpha=.5)) + scale_size_area(max_size=12) + stat_smooth(method= "lm", formula = y ~ ns(x,3)) + scale_alpha_continuous(guide="none", limits = c(0,.7)) + scale_color_brewer(palette = "Set1") + ylim(0,1)



#THIS IS NEW:

p <- ggplot(ex.data, aes(Year, OV, color=sbj, group=sbj)) + labs(y = "Proportion of OV", x = "\nYear") + stat_sum(aes(size=..n.., alpha=.5)) + scale_size_area(max_size=12) + stat_smooth() + scale_alpha_continuous(guide="none", limits = c(0,.7)) + scale_color_brewer(palette = "Set1") + ylim(0,1) + facet_wrap(~Clause)

ggsave(p, file = "infoTheoryTest.pdf", width = 8, height = 5)



plot.data <- ddply(ex.data, .(Year,Clause),summarize, ov = mean(OV, na.rm = T), n = sum(!is.na(OV)))

p <- ggplot(plot.data, aes(Year, ov, color=Clause, group=Clause)) + labs(y = "Proportion of OV", x = "\nYear") + stat_sum(aes(size=..n.., alpha=.5)) + geom_point(aes(size=n)) + scale_size_area(max_size=12) + stat_smooth() + scale_alpha_continuous(guide="none", limits = c(0,.7)) + scale_color_brewer(palette = "Set1") + ylim(0,1)


ggsave(p, file = "myovvoByYearLoess.pdf", width = 8, height = 5)

ex.data$Time2 <- floor(ex.data$Year/50)*50

plot.data <- ddply(ex.data, .(Time2,Clause),summarize, ov = mean(OV, na.rm = T), n = sum(!is.na(OV)))

p <- ggplot(plot.data, aes(Time2, ov, color=Clause, group=Clause)) + labs(y = "Proportion of OV", x = "\nYear") + stat_sum(aes(size=..n.., alpha=.5)) + geom_point(aes(size=n)) + scale_size_area(max_size=12) + stat_smooth() + scale_alpha_continuous(guide="none", limits = c(0,.7)) + scale_color_brewer(palette = "Set1") + ylim(0,1)


ggsave(p, file = "myovvoBinnedLoess.pdf", width = 8, height = 5)

plot.data <- ddply(ex.data, .(Year,Clause),summarize, ov = mean(OV, na.rm = T), n = sum(!is.na(OV)))

p <- ggplot(plot.data, aes(Year, ov, color=Clause, group=Clause)) + labs(y = "Proportion of OV", x = "\nYear") + stat_sum(aes(size=..n.., alpha=.5)) + scale_size_area(max_size=12) + stat_smooth(method= "lm", formula = y ~ ns(x,4)) + scale_alpha_continuous(guide="none", limits = c(0,.7)) + scale_color_brewer(palette = "Set1") + ylim(0,1) + geom_point(aes(size=n))


ggsave(p, file = "myovvoByYearSpline.pdf", width = 8, height = 5)