#note that this script uses the uido and dorm functions from dormUido.R in the same directory


#Reads info content strings for each sentence in pyccle into a data frame; I've had to partition pyccle's bits into 5 parts and read each in one at a time or R will crash
pyccle.bits <- 0
pyccle.bits <- readLines(con=(file="~/CurrentLx/infoTheory/localCailScripts/x05"))




#Creates a vector of vectors for each sentence in pyccle that is 10 words long

pyccle.vecs <- list()

ii <- 1

while (ii <= length(pyccle.bits))
{
  sentence <- strsplit(pyccle.bits[ii],",")
  #the split function returns a list, not a vector, hence the [[]] notation below cause R is weird about lists
  if (length(sentence[[1]]) == 10)
  {
    sentence[[1]] <- as.numeric(sentence[[1]])
    pyccle.vecs <- append(pyccle.vecs,sentence)
  }
  ii <- ii+1
}

#i needed to save the vector in two halves so as to clear working memory between runs of the above, so as not to crash my laptop
pyccle.vecs.df <- as.data.frame(do.call(rbind, pyccle.vecs))
write.csv(file= "~/CurrentLx/infoTheory/localCailScripts/pyccle.vecs2.df",pyccle.vecs.df)

pyccle.vecs1.df <- read.csv(file="~/CurrentLx/infoTheory/localCailScripts/pyccle.vecs1.df", header=F, row.names = 1)

colnames(pyccle.vecs1.df) <- c("V1","V2","V3","V4","V5","V6","V7","V8","V9","V10")

pyccle.vecs2.df <- read.csv(file="~/CurrentLx/infoTheory/localCailScripts/pyccle.vecs2.df", header=F, row.names = 1)

colnames(pyccle.vecs2.df) <- c("V1","V2","V3","V4","V5","V6","V7","V8","V9","V10")

pyccle.vecsall.df <- rbind(pyccle.vecs1.df,pyccle.vecs2.df)

#convert the dataframe back to a list of vectors for use in the sim below
pyccle.vecs.list <- as.list(as.data.frame(t(pyccle.vecsall.df)))
  


ii = 1 #initialize counter
currenttrial = 1 #initialize trial counter

#initialize vectors for each component of each run of the loop

niter <- length(pyccle.vecs.list) 

noises <- vector(length = niter) #proportion of total info in the sentence destroyed by noise
stringtypes <- vector(length = niter)
trials <- vector(length = niter)
absNoise <- vector(length = niter) #absolute amount of info in the sentence destroyed by noise, in bits
dorms <- vector(length = niter) #dorm for each array, using "dorm" function from dormUido.R




while (currenttrial <= niter)
{
  
  probvecinfo <- pyccle.vecs.list[[currenttrial]]
  
  probvec <- 1/(2^probvecinfo)
  
  starting <- sample(1:(length(probvec)-2), size=1) #remember, lists in R perversely start at offset 1, not 0
  
  noise <- c(probvec[starting], probvec[starting+1], probvec[starting+2])
  noiseinfo <- log2(1/noise)
  probvecinfo <- log2(1/probvec)
  totalnoise <- sum(noiseinfo)
  totalprobvecinfo <- sum(probvecinfo)
  noiseprop <- totalnoise/totalprobvecinfo
  probvecDorm <- dorm(probvecinfo)
  

  
#Doing the same thing, but now the noise will fall in the same position in a vector that is end-heavy for information, but contains the same total info
  lopsided <- sort(probvec)

#It is important that the starting point in the vector is the same for both cases, so that each comparison of noise is a true comparison
  noise2 <- c(lopsided[starting], lopsided[starting+1], lopsided[starting+2])
  noise2info <- log2(1/noise2)
  lopsidedinfo <- log2(1/lopsided)
  totalnoise2 <- sum(noise2info)
  lopsidedDorm <- dorm(lopsidedinfo)

#This next is really a sanity check: this vector contains the same probabilities as the last, so this should be equal to totalprobvecinfo
  totallopsidedinfo <- sum(lopsidedinfo)
  noise2prop <- totalnoise2/totallopsidedinfo

  
  #doing the same thing, but for a vector optimized by the uido algorithm
  uidoinfo <- uido(probvecinfo)
  
  totalnoise4 <- sum(c(uidoinfo[starting], uidoinfo[starting+1], uidoinfo[starting+2]))
  totaluidoinfo <- sum(uidoinfo)
  noise4prop <- totalnoise4/totaluidoinfo
  uidoDorm <- dorm(uidoinfo)
  
  
  #doing the same thing, but for a randomized vector
  randominfo <- sample(probvecinfo)
  
  totalnoiseRand <- sum(c(randominfo[starting], randominfo[starting+1], randominfo[starting+2]))
  totalrandominfo <- sum(randominfo)
  noiseRandprop <- totalnoiseRand/totalrandominfo
  randomDorm <- dorm(randominfo)
  
  
  #update vectors for each trial
  noises[ii] <- noiseprop
  absNoise[ii] <- totalnoise
  stringtypes[ii] <- "original"
  trials[ii] <- currenttrial
  dorms[ii] <- probvecDorm
  
  noises[ii+1] <- noise2prop
  absNoise[ii+1] <- totalnoise2
  stringtypes[ii+1] <- "asymmetric"
  trials[ii+1] <- currenttrial
  dorms[ii+1] <- lopsidedDorm
  
  noises[ii+2] <- noise4prop
  absNoise[ii+2] <- totalnoise4
  stringtypes[ii+2] <- "uido-optimized"
  trials[ii+2] <- currenttrial
  dorms[ii+2] <- uidoDorm
  
  noises[ii+3] <- noiseRandprop
  absNoise[ii+3] <- totalnoiseRand
  stringtypes[ii+3] <- "randomized"
  trials[ii+3] <- currenttrial
  dorms[ii+3] <- randomDorm
  
  ii <- ii+4
  currenttrial <- currenttrial+1 
}

#bind vectors into a data frame and add column names
propnoise.df <- data.frame(trials,noises,absNoise,stringtypes,dorms)
#colnames(propnoise.df) <- c("Trial","NoiseProp","VecType")


propnoise.df$trials <- as.numeric(as.character(propnoise.df$trials))
propnoise.df$noises <- as.numeric(as.character(propnoise.df$noises))
propnoise.df$dorms <- as.numeric(as.character(propnoise.df$dorms))

#add a column into the dataframe recoding for losing more than 50% of the info

propnoise.df$bigLoss <- ifelse(propnoise.df$noises >= 0.5, 1, 0)

write.csv(propnoise.df,file = "~/constantentropy/uid_sentSimWithRand.csv", row.names = F)
propnoise.df <- read.csv(file = "~/constantentropy/uid_sentSimWithRand.csv", header=T)

propnoise.df$stringtypes <- ordered(propnoise.df$stringtypes, levels = c("asymmetric","randomized","original","uido-optimized"))



#Graphing

library(ggplot2)

#dorm by proportion bits lost

ggplot(propnoise.df, aes(dorms, noises, group=stringtypes)) + 
  scale_y_continuous(name = "Proportion of Total Bits Lost For Each \"Sentence\"") + 
  scale_x_discrete(name = "\nDorm") + 
 # geom_point(alpha = 1/25) + 
  geom_smooth() +
  #geom_jitter(width = 0.3) +
  theme_bw() + 
  theme(panel.border = element_blank())

#box plot

p <- ggplot(propnoise.df, aes(stringtypes, noises, group=stringtypes)) + 
  scale_y_continuous(name = "Proportion of Total Bits Lost For Each \"Sentence\"") + 
  scale_x_discrete(name = "\nOrder") + 
  geom_point(alpha = 1/25) + 
  geom_boxplot(fill=c("purple","green","red","yellow")) +
  #geom_jitter(width = 0.3) +
  theme_bw() + 
  theme(panel.border = element_blank())

ggsave(p, file = "~/constantentropy/uid-sentSim-totalbits.png", width = 8.09, height = 5)


#plotting bigLoss in a bar chart



#####bar plot
propnoise.bar.df <- data.frame(stringtypes=c("asymmetric","randomized","original","uido-optimized"),NoSentences=c(sum(propnoise.df[stringtypes=="asymmetric",]$bigLoss),sum(propnoise.df[stringtypes=="randomized",]$bigLoss),sum(propnoise.df[stringtypes=="original",]$bigLoss),sum(propnoise.df[stringtypes=="uido-optimized",]$bigLoss)))

propnoise.bar.df$stringtypes <- ordered(propnoise.bar.df$stringtypes, levels = c("asymmetric","randomized","original","uido-optimized"))

q <- ggplot(propnoise.bar.df, aes(stringtypes,NoSentences)) + 
  scale_y_continuous(name = "Number of Sentences with > 50% Information Lost", limits = c(0,10000) ) + 
  scale_x_discrete(name = "\nOrder") + 
  #geom_point(alpha = 1/25) + 
  geom_bar(fill=c("purple","green","red","yellow"), color="black", stat="identity") +
  #geom_jitter(width = 0.3) +
  theme_bw() + 
  theme(panel.border = element_blank())

ggsave(q, file = "~/constantentropy/uid-sentSim-majority.png", width = 8.09, height = 5)


#box plot of dorms by stringtype

k <- ggplot(propnoise.df, aes(stringtypes, dorms, group=stringtypes)) + 
  scale_y_continuous(name = "DORM for each sentence-version") + 
  scale_x_discrete(name = "\nOrder") + 
  geom_point(alpha = 1/25) + 
  geom_boxplot(fill=c("purple","green","red","yellow")) +
  #geom_jitter(width = 0.3) +
  theme_bw() + 
  theme(panel.border = element_blank())

ggsave(k, file = "~/constantentropy/uid-sentSim-dorms.png", width = 8.09, height = 5)



#modeling
#get rid of the ordering of the levels of stringtypes, as that was just for plotting purposes
propnoise.df$stringtypes <- as.factor(propnoise.df$stringtypes)

library(lme4)
#simple linear
totalbits.fit <- lm(absNoise~stringtypes, data=propnoise.df)
summary(totalbits.fit)

#linear with DORM as predictor
totalbits.fit2 <- lm(absNoise~dorms, data=propnoise.df)
summary(totalbits.fit2)

#linear for DORM by stringtypes
totalbits.fit3 <- lm(dorms~stringtypes, data=propnoise.df)
summary(totalbits.fit3)



#simple logistic
failure.fit <- glm(bigLoss~stringtypes, family=binomial, data=propnoise.df)
summary(failure.fit)

#logistic with DORM as predictor instead
failure.fit2 <- glm(bigLoss~dorms, family=binomial, data=propnoise.df)
summary(failure.fit2)



#change contrasts so we can compare uido to randomized
propnoise.df$stringtypes <- relevel(propnoise.df$stringtypes, ref="original")

failure.relevel.fit <- glm(bigLoss~stringtypes, family=binomial, data=propnoise.df)
summary(failure.relevel.fit)


#Estimate variances on the Zed values for info lost to noise for each string type:
var(propnoise.df[propnoise.df$stringtypes == "uido-optimized",]$zAbsNoise)
var(propnoise.df[propnoise.df$stringtypes == "random",]$zAbsNoise)
var(propnoise.df[propnoise.df$stringtypes == "asymmetric",]$zAbsNoise)

#######################
####Random noise simulation; same as above, but the three noise events do not have to be sequential (though they may be)


ii = 1 #initialize counter
currenttrial = 1 #initialize trial counter

#initialize vectors for each component of each run of the loop

niter <- length(pyccle.vecs.list) 

noises <- vector(length = niter) #proportion of total info in the sentence destroyed by noise
stringtypes <- vector(length = niter)
trials <- vector(length = niter)
absNoise <- vector(length = niter) #absolute amount of info in the sentence destroyed by noise, in bits
dorms <- vector(length = niter) #dorm for each array, using "dorm" function from dormUido.R




while (currenttrial <= niter)
{
  
  probvecinfo <- pyccle.vecs.list[[currenttrial]]
  
  probvec <- 1/(2^probvecinfo)
  
  #Now unlike the simulation above, this just selects three string positions from anywhere in the string, without replacement
  noisePositions <- sample(1:length(probvec), size=3, replace=FALSE) #remember, lists in R perversely start at offset 1, not 0
  
  noise <- c(probvec[noisePositions[1]], probvec[noisePositions[2]], probvec[noisePositions[3]])
  noiseinfo <- log2(1/noise)
  probvecinfo <- log2(1/probvec)
  totalnoise <- sum(noiseinfo)
  totalprobvecinfo <- sum(probvecinfo)
  noiseprop <- totalnoise/totalprobvecinfo
  probvecDorm <- dorm(probvecinfo)
  
  
  
  #Doing the same thing, but now the noise will fall in the same position in a vector that is end-heavy for information, but contains the same total info
  lopsided <- sort(probvec)
  
  #It is important that the starting point in the vector is the same for both cases, so that each comparison of noise is a true comparison
  noise2 <- c(lopsided[noisePositions[1]], lopsided[noisePositions[2]], lopsided[noisePositions[3]])
  noise2info <- log2(1/noise2)
  lopsidedinfo <- log2(1/lopsided)
  totalnoise2 <- sum(noise2info)
  lopsidedDorm <- dorm(lopsidedinfo)
  
  #This next is really a sanity check: this vector contains the same probabilities as the last, so this should be equal to totalprobvecinfo
  totallopsidedinfo <- sum(lopsidedinfo)
  noise2prop <- totalnoise2/totallopsidedinfo
  
  
  #doing the same thing, but for a vector optimized by the uido algorithm
  uidoinfo <- uido(probvecinfo)
  
  totalnoise4 <- sum(c(uidoinfo[noisePositions[1]], uidoinfo[noisePositions[2]], uidoinfo[noisePositions[3]]))
  totaluidoinfo <- sum(uidoinfo)
  noise4prop <- totalnoise4/totaluidoinfo
  uidoDorm <- dorm(uidoinfo)
  
  
  #doing the same thing, but for a randomized vector
  randominfo <- sample(probvecinfo)
  
  totalnoiseRand <- sum(c(randominfo[noisePositions[1]], randominfo[noisePositions[2]], randominfo[noisePositions[3]]))
  totalrandominfo <- sum(randominfo)
  noiseRandprop <- totalnoiseRand/totalrandominfo
  randomDorm <- dorm(randominfo)
  
  
  #update vectors for each trial
  noises[ii] <- noiseprop
  absNoise[ii] <- totalnoise
  stringtypes[ii] <- "original"
  trials[ii] <- currenttrial
  dorms[ii] <- probvecDorm
  
  noises[ii+1] <- noise2prop
  absNoise[ii+1] <- totalnoise2
  stringtypes[ii+1] <- "asymmetric"
  trials[ii+1] <- currenttrial
  dorms[ii+1] <- lopsidedDorm
  
  noises[ii+2] <- noise4prop
  absNoise[ii+2] <- totalnoise4
  stringtypes[ii+2] <- "uido-optimized"
  trials[ii+2] <- currenttrial
  dorms[ii+2] <- uidoDorm
  
  noises[ii+3] <- noiseRandprop
  absNoise[ii+3] <- totalnoiseRand
  stringtypes[ii+3] <- "randomized"
  trials[ii+3] <- currenttrial
  dorms[ii+3] <- randomDorm
  
  ii <- ii+4
  currenttrial <- currenttrial+1 
}

#bind vectors into a data frame and add column names
propnoise.df <- data.frame(trials,noises,absNoise,stringtypes,dorms)
#colnames(propnoise.df) <- c("Trial","NoiseProp","VecType")


propnoise.df$trials <- as.numeric(as.character(propnoise.df$trials))
propnoise.df$noises <- as.numeric(as.character(propnoise.df$noises))
propnoise.df$dorms <- as.numeric(as.character(propnoise.df$dorms))

#add a column into the dataframe recoding for losing more than 50% of the info

propnoise.df$bigLoss <- ifelse(propnoise.df$noises >= 0.5, 1, 0)

write.csv(propnoise.df,file = "~/constantentropy/uid_sentSimOneWordNoises.csv", row.names = F)
propnoise.df <- read.csv(file = "~/constantentropy/uid_sentSimOneWordNoises.csv", header=T)

propnoise.df$stringtypes <- ordered(propnoise.df$stringtypes, levels = c("asymmetric","randomized","original","uido-optimized"))