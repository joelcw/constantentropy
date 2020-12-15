#This script creates vectors of various lengths consisting of random, independent information content values,
#and calculates their dorm() and uido(dorm()) - dorm() for comparison.

library(devtools)
source_url('https://raw.githubusercontent.com/rbailes/DORM-UIDO/f778fd66c30ce8a127dfac526084db87abeb8cdc/dormUido.R')




minStringLength = 10 #first length to be considered
maxStringLength = 100 #longest string to be considered
ii <- 1 #initialize counter
niter <- 1000 #number iterations
current = 1 #initialize iteration counter



foo <- matrix(nc=5,nr=((maxStringLength-minStringLength+1)*niter),0)

strings.df <- data.frame(foo)

colnames(strings.df) <- c("Iteration","Lengths","Dorm","Uido","DormUido")



while (current <= niter)
{
  
  stringLength <- minStringLength
  
  while (stringLength <= maxStringLength)
    {
      
      string <- -log2(runif(n=stringLength,min=0,max=1))
      stringDorm <- dorm(string)
      stringUido <- dorm(uido(string))
      stringDormUido <- stringDorm-stringUido
      
      strings.df$Iteration[ii] <- current
      strings.df$Lengths[ii] <- stringLength
      strings.df$Dorm[ii] <- stringDorm
      strings.df$Uido[ii] <- stringUido
      strings.df$DormUido[ii] <- stringDormUido

      
      stringLength <- stringLength+1
      ii <- ii+1
    }
  
  current <- current+1    
  
}


write.csv(strings.df,file="~/constantentropy/dormLengthSim.csv")
strings.df <- read.csv(file="~/constantentropy/dormLengthSim.csv", header=T)


####Same sim, but using frequencies from Brown
library(zipfR)
Brown.probs <- Brown.tfl$f/N(Brown.tfl) #vector of word probabilities derived from the word freqs of the Brown corpus


minStringLength = 10 #first length to be considered
maxStringLength = 100 #longest string to be considered
ii <- 1 #initialize counter
niter <- 1000 #number iterations
current = 1 #initialize iteration counter



foo <- matrix(nc=5,nr=((maxStringLength-minStringLength+1)*niter),0)

strings.df <- data.frame(foo)

colnames(strings.df) <- c("Iteration","Lengths","Dorm","Uido","DormUido")



while (current <= niter)
{
  
  stringLength <- minStringLength
  
  while (stringLength <= maxStringLength)
  {
    
    string <- -log2(sample(Brown.probs, stringLength, replace = T, prob = Brown.probs))
    stringDorm <- dorm(string)
    stringUido <- dorm(uido(string))
    stringDormUido <- stringDorm-stringUido
    
    strings.df$Iteration[ii] <- current
    strings.df$Lengths[ii] <- stringLength
    strings.df$Dorm[ii] <- stringDorm
    strings.df$Uido[ii] <- stringUido
    strings.df$DormUido[ii] <- stringDormUido
    
    
    stringLength <- stringLength+1
    ii <- ii+1
  }
  
  current <- current+1    
  
}

write.csv(strings.df,file="~/constantentropy/dormLengthZipfSim.csv")
strings2.df <- read.csv(file="~/constantentropy/dormLengthZipfSim.csv", header=T)

#Graphing

library(ggplot2)
library(reshape2)
library(RColorBrewer)

plot.df <- melt(data = strings.df, id.vars = c("X", "Iteration","Lengths"), measure.vars = c("Dorm", "Uido","DormUido"))

p <- ggplot(plot.df, aes(Lengths, value, color=variable)) + 
  #scale_y_continuous(limits = c(0,2)) +
  labs(y = "Bits", x = "\nLength of Array", color="") +
  geom_smooth() +
  theme_bw() + theme(panel.border = element_blank()) +
  theme(legend.position = c(0.6, 0.4),
        legend.direction = "vertical") +
  scale_color_hue(l=40, labels = c("DORM(random array)","DORM(UIDO)","DORM(random array) - DORM(UIDO)"))


ggsave(p, file = "~/constantentropy/dormLengthSim.png", width = 8.09, height = 5)


plot2.df <- melt(data = strings2.df, id.vars = c("X", "Iteration","Lengths"), measure.vars = c("Dorm", "Uido","DormUido"))


q <- ggplot(plot2.df, aes(Lengths, value, color=variable)) + 
  labs(y = "Bits", x = "\nLength of Array", color="") +
  #scale_y_continuous(limits = c(0,8)) +
  geom_smooth() +
  #geom_point(alpha = 0.1) +
  theme_bw() + theme(panel.border = element_blank()) +
  theme(legend.position = c(0.54, 0.41),
        legend.direction = "vertical") +
  scale_color_hue(l=40, labels = c("DORM(random array)","DORM(UIDO)","DORM(random array) - DORM(UIDO)"))
  #scale_colour_manual(values = c("red","blue","green"), labels = c("DORM(random array)", "DORM(random array) - DORM(UIDO)","DORM(UIDO)"))
  
ggsave(q, file = "~/constantentropy/dormLengthZipfSim.png", width = 8.09, height = 5)





#####simple linear for uniform distribution
dorm.fit <- lm(Dorm~Lengths, data=strings.df)
summary(dorm.fit)

#linear for dorm(uido())
uido.fit <- lm(Uido~Lengths, data=strings.df)
summary(uido.fit)

#linear for DORM-UIDO
dormuido.fit <- lm(DormUido~Lengths, data=strings.df)
summary(dormuido.fit)


#####simple linear for Zipfian distribution
dorm.fit <- lm(Dorm~Lengths, data=strings2.df)
summary(dorm.fit)

#linear for dorm(uido())
uido.fit <- lm(Uido~Lengths, data=strings2.df)
summary(uido.fit)

#linear for DORM-UIDO
dormuido.fit <- lm(DormUido~Lengths, data=strings2.df)
summary(dormuido.fit)
