#note that this script uses the uido and dorm functions from dormUido.R in the same directory

library(zipfR)
Brown.probs <- Brown.tfl$f/N(Brown.tfl) #vector of word probabilities derived from the word freqs of the Brown corpus

ii = 1 #initialize counter
currenttrial = 1 #initialize trial counter

#initialize vectors for each component of each run of the loop

niter <- 100000

noises <- vector(length = niter) #proportion of total info in the sentence destroyed by noise
stringtypes <- vector(length = niter)
trials <- vector(length = niter)
absNoise <- vector(length = niter) #absolute amount of info in the sentence destroyed by noise, in bits
dorms <- vector(length = niter) #dorm for each array, using "dorm" function from dormUido.R
autoCovs <- vector(length = niter)



while (currenttrial <= niter)
{
  #probvec <- runif(10,min=0,max=1) #Old way: probs from a uniform distribution
  probvec <- sample(Brown.probs, 10, replace = T, prob = Brown.probs) #probabilities randomly sampled from the Brown corpus probs, choice waited by their actual probabilities
  
  starting <- sample(1:(length(probvec)-2), size=1) #remember, lists in R perversely start at offset 1, not 0
  
  noise <- c(probvec[starting], probvec[starting+1], probvec[starting+2])
  noiseinfo <- log2(1/noise)
  probvecinfo <- log2(1/probvec)
  totalnoise <- sum(noiseinfo)
  totalprobvecinfo <- sum(probvecinfo)
  noiseprop <- totalnoise/totalprobvecinfo
  probvecDorm <- dorm(probvecinfo)
  probveccov <- abs(acf(probvecinfo, type = "covariance", plot = FALSE)[[1]][length(probvecinfo)])

  
#Doing the same thing, but now the noise will fall in the same position in a vector that is end-heavy for information, but contains the same total info
  lopsided <- sort(probvec)

#It is important that the starting point in the vector is the same for both cases, so that each comparison of noise is a true comparison
  noise2 <- c(lopsided[starting], lopsided[starting+1], lopsided[starting+2])
  noise2info <- log2(1/noise2)
  lopsidedinfo <- log2(1/lopsided)
  totalnoise2 <- sum(noise2info)
  lopsidedDorm <- dorm(lopsidedinfo)
  lopsidedcov <- abs(acf(lopsidedinfo, type = "covariance", plot = FALSE)[[1]][length(lopsidedinfo)])

#This next is really a sanity check: this vector contains the same probabilities as the last, so this should be equal to totalprobvecinfo
  totallopsidedinfo <- sum(lopsidedinfo)
  noise2prop <- totalnoise2/totallopsidedinfo

  
  #doing the same thing, but for a vector optimized by the uido algorithm
  
  uidoinfo <- uido(probvecinfo)
  totalnoise4 <- sum(c(uidoinfo[starting], uidoinfo[starting+1], uidoinfo[starting+2]))
  totaluidoinfo <- sum(uidoinfo)
  noise4prop <- totalnoise4/totaluidoinfo
  uidoDorm <- dorm(uidoinfo)
  uidocov <- abs(acf(uidoinfo, type = "covariance", plot = FALSE)[[1]][length(uidoinfo)])
  
  #same thing, but with autocovariance version of uido:
  uidoAutoCovInfo <- uidoAutoCov(probvecinfo)
  totalNoiseCov <- sum(c(uidoAutoCovInfo[starting], uidoAutoCovInfo[starting+1], uidoAutoCovInfo[starting+2]))
  totalAutoCovInfo <- sum(uidoAutoCovInfo)
  noiseAutoCovProp <- totalNoiseCov/totalAutoCovInfo
  uidoAutoCovDorm <- dorm(uidoAutoCovInfo)
  uidoAutoCovcov <- abs(acf(uidoAutoCovInfo, type = "covariance", plot = FALSE)[[1]][length(uidoAutoCovInfo)])
  
  #update vectors for each trial
  noises[ii] <- noiseprop
  absNoise[ii] <- totalnoise
  stringtypes[ii] <- "random"
  trials[ii] <- currenttrial
  dorms[ii] <- probvecDorm
  autoCovs[ii] <- probveccov
  
  
  noises[ii+1] <- noise2prop
  absNoise[ii+1] <- totalnoise2
  stringtypes[ii+1] <- "asymmetric"
  trials[ii+1] <- currenttrial
  dorms[ii+1] <- lopsidedDorm
  autoCovs[ii+1] <- lopsidedcov
  
  noises[ii+2] <- noise4prop
  absNoise[ii+2] <- totalnoise4
  stringtypes[ii+2] <- "uido-dorm"
  trials[ii+2] <- currenttrial
  dorms[ii+2] <- uidoDorm
  autoCovs[ii+2] <- uidocov
  
  noises[ii+3] <- noiseAutoCovProp
  absNoise[ii+3] <- totalNoiseCov
  stringtypes[ii+3] <- "uido-autoCov"
  trials[ii+3] <- currenttrial
  dorms[ii+3] <- uidoAutoCovDorm
  autoCovs[ii+3] <- uidoAutoCovcov
  
  ii <- ii+4
  currenttrial <- currenttrial+1 
}

#bind vectors into a data frame and add column names
propnoise.df <- data.frame(trials,noises,absNoise,stringtypes,dorms,autoCovs)
#colnames(propnoise.df) <- c("Trial","NoiseProp","VecType")


#Graphing

library(ggplot2)

propnoise.df$trials <- as.numeric(as.character(propnoise.df$trials))
propnoise.df$noises <- as.numeric(as.character(propnoise.df$noises))
propnoise.df$dorms <- as.numeric(as.character(propnoise.df$dorms))
propnoise.df$autoCovs <- as.numeric(as.character(propnoise.df$autoCovs))

#add a column into the dataframe recoding for losing more than 50% of the info

propnoise.df$bigLoss <- ifelse(propnoise.df$noises >= 0.5, 1, 0)

write.csv(propnoise.df,file = "~/constantentropy/uid_sim_cov_100k.csv", row.names = F, col.names = T)


#box plot

p <- ggplot(propnoise.df, aes(stringtypes, noises, group=stringtypes)) + 
  scale_y_continuous(name = "Proportion of Total Bits Lost For Each \"Sentence\"") + 
  scale_x_discrete(name = "\nOrder") + 
  geom_point(alpha = 1/25) + 
  geom_boxplot(fill=c("purple","green","yellow","red")) +
  #geom_jitter(width = 0.3) +
  theme_bw() + 
  theme(panel.border = element_blank())

#ggsave(p, file = "~/constantentropy/uid-sim-totalbits1mil.png", width = 8.09, height = 5)


#plotting bigLoss in a bar chart


#####New way
propnoise.bar.df <- data.frame(stringtypes=c("asymmetric","random","uido-dorm","uido-autoCov"),NoSentences=c(sum(propnoise.df[stringtypes=="asymmetric",]$bigLoss),sum(propnoise.df[stringtypes=="random",]$bigLoss),sum(propnoise.df[stringtypes=="uido-dorm",]$bigLoss),sum(propnoise.df[stringtypes=="uido-autoCov",]$bigLoss)))

q <- ggplot(propnoise.bar.df, aes(stringtypes,NoSentences)) + 
  scale_y_continuous(name = "Number of Sentences with > 50% Information Lost", limits = c(0,10000) ) + 
  scale_x_discrete(name = "\nOrder") + 
  #geom_point(alpha = 1/25) + 
  geom_bar(fill=c("purple","green","yellow","red"), color="black", stat="identity") +
  #geom_jitter(width = 0.3) +
  theme_bw() + 
  theme(panel.border = element_blank())

#ggsave(q, file = "~/constantentropy/uid-sim-majority1mil.png", width = 8.09, height = 5)


#box plot of dorms by stringtype

k <- ggplot(propnoise.df, aes(stringtypes, dorms, group=stringtypes)) + 
  scale_y_continuous(name = "DORM for each sentence-version") + 
  scale_x_discrete(name = "\nOrder") + 
  geom_point(alpha = 1/25) + 
  geom_boxplot(fill=c("purple","green","yellow","red")) +
  #geom_jitter(width = 0.3) +
  theme_bw() + 
  theme(panel.border = element_blank())

ggsave(k, file = "~/constantentropy/uid-sentSim-dorms.png", width = 8.09, height = 5)

#plot of autocovariances by stringtype

ggplot(propnoise.df, aes(stringtypes, autoCovs, group=stringtypes)) + 
  scale_y_continuous(name = "DORM for each sentence-version") + 
  scale_x_discrete(name = "\nOrder") + 
  geom_point(alpha = 1/25) + 
  geom_boxplot(fill=c("purple","green","yellow","red")) +
  #geom_jitter(width = 0.3) +
  theme_bw() + 
  theme(panel.border = element_blank())


#modeling

library(lme4)
#simple linear
totalbits.fit <- lm(absNoise~stringtypes, data=propnoise.df)
summary(totalbits.fit)

#linear with random effect of trial
totalbits.randfit <- lmer(absNoise~stringtypes+(1|trials), data=propnoise.df)
summary(totalbits.randfit)

#rescaled for 1 million trials with zipfian, Zing the numeric predictors so the mixed effects model doesnt barf so much:

propnoise.df$zAbsNoise <- scale(propnoise.df$absNoise, center=TRUE, scale=TRUE)

#linear with random effect of trial, using Zed absNoise
totalbits.randfit <- lmer(zAbsNoise~stringtypes+(1|trials), data=propnoise.df)
summary(totalbits.randfit)


#simple logistic
failure.fit <- glm(bigLoss~stringtypes, family=binomial, data=propnoise.df)
summary(failure.fit)


#logistic with random effect of trial
failure.fit <- glmer(bigLoss~stringtypes+(1|trials), family=binomial, data=propnoise.df)
summary(failure.fit)



#change contrasts so we can compare uido to randomized
propnoise.df$stringtypes <- relevel(propnoise.df$stringtypes, ref="original")
failure.relevel.fit <- glmer(bigLoss~stringtypes+(1|trials), family=binomial, data=propnoise.df)
summary(failure.relevel.fit)

failure.relevel.fit <- glm(bigLoss~stringtypes, family=binomial, data=propnoise.df)
summary(failure.relevel.fit)


#Estimate variances on the Zed values for info lost to noise for each string type:
var(propnoise.df[propnoise.df$stringtypes == "uido-dorm",]$zAbsNoise)
var(propnoise.df[propnoise.df$stringtypes == "random",]$zAbsNoise)
var(propnoise.df[propnoise.df$stringtypes == "asymmetric",]$zAbsNoise)



#####repeat the sim with a longer stretch of noise


ii = 1 #initialize counter
currenttrial = 1 #initialize trial counter

#initialize vectors for each component of each run of the loop

niter <- 10000

noises <- vector(length = niter) #proportion of total info in the sentence destroyed by noise
stringtypes <- vector(length = niter)
trials <- vector(length = niter)
absNoise <- vector(length = niter) #absolute amount of info in the sentence destroyed by noise, in bits
dorms <- vector(length = niter) #dorm for each array, using "dorm" function from dormUido.R
autoCovs <- vector(length = niter)



while (currenttrial <= niter)
{
  #probvec <- runif(10,min=0,max=1) #Old way: probs from a uniform distribution
  probvec <- sample(Brown.probs, 10, replace = T, prob = Brown.probs) #probabilities randomly sampled from the Brown corpus probs, choice waited by their actual probabilities
  
  starting <- sample(1:(length(probvec)-3), size=1) #remember, lists in R perversely start at offset 1, not 0
  
  noise <- c(probvec[starting], probvec[starting+1], probvec[starting+2], probvec[starting+3])
  noiseinfo <- log2(1/noise)
  probvecinfo <- log2(1/probvec)
  totalnoise <- sum(noiseinfo)
  totalprobvecinfo <- sum(probvecinfo)
  noiseprop <- totalnoise/totalprobvecinfo
  probvecDorm <- dorm(probvecinfo)
  probveccov <- abs(acf(probvecinfo, type = "covariance", plot = FALSE)[[1]][length(probvecinfo)])
  
  
  #Doing the same thing, but now the noise will fall in the same position in a vector that is end-heavy for information, but contains the same total info
  lopsided <- sort(probvec)
  
  #It is important that the starting point in the vector is the same for both cases, so that each comparison of noise is a true comparison
  noise2 <- c(lopsided[starting], lopsided[starting+1], lopsided[starting+2], lopsided[starting+3])
  noise2info <- log2(1/noise2)
  lopsidedinfo <- log2(1/lopsided)
  totalnoise2 <- sum(noise2info)
  lopsidedDorm <- dorm(lopsidedinfo)
  lopsidedcov <- abs(acf(lopsidedinfo, type = "covariance", plot = FALSE)[[1]][length(lopsidedinfo)])
  
  #This next is really a sanity check: this vector contains the same probabilities as the last, so this should be equal to totalprobvecinfo
  totallopsidedinfo <- sum(lopsidedinfo)
  noise2prop <- totalnoise2/totallopsidedinfo
  
  
  #doing the same thing, but for a vector optimized by the uido algorithm
  
  uidoinfo <- uido(probvecinfo)
  totalnoise4 <- sum(c(uidoinfo[starting], uidoinfo[starting+1], uidoinfo[starting+2], uidoinfo[starting+3]))
  totaluidoinfo <- sum(uidoinfo)
  noise4prop <- totalnoise4/totaluidoinfo
  uidoDorm <- dorm(uidoinfo)
  uidocov <- abs(acf(uidoinfo, type = "covariance", plot = FALSE)[[1]][length(uidoinfo)])
  
  #same thing, but with autocovariance version of uido:
  uidoAutoCovInfo <- uidoAutoCov(probvecinfo)
  totalNoiseCov <- sum(c(uidoAutoCovInfo[starting], uidoAutoCovInfo[starting+1], uidoAutoCovInfo[starting+2],uidoAutoCovInfo[starting+3]))
  totalAutoCovInfo <- sum(uidoAutoCovInfo)
  noiseAutoCovProp <- totalNoiseCov/totalAutoCovInfo
  uidoAutoCovDorm <- dorm(uidoAutoCovInfo)
  uidoAutoCovcov <- abs(acf(uidoAutoCovInfo, type = "covariance", plot = FALSE)[[1]][length(uidoAutoCovInfo)])
  
  #update vectors for each trial
  noises[ii] <- noiseprop
  absNoise[ii] <- totalnoise
  stringtypes[ii] <- "random"
  trials[ii] <- currenttrial
  dorms[ii] <- probvecDorm
  autoCovs[ii] <- probveccov
  
  
  noises[ii+1] <- noise2prop
  absNoise[ii+1] <- totalnoise2
  stringtypes[ii+1] <- "asymmetric"
  trials[ii+1] <- currenttrial
  dorms[ii+1] <- lopsidedDorm
  autoCovs[ii+1] <- lopsidedcov
  
  noises[ii+2] <- noise4prop
  absNoise[ii+2] <- totalnoise4
  stringtypes[ii+2] <- "uido-dorm"
  trials[ii+2] <- currenttrial
  dorms[ii+2] <- uidoDorm
  autoCovs[ii+2] <- uidocov
  
  noises[ii+3] <- noiseAutoCovProp
  absNoise[ii+3] <- totalNoiseCov
  stringtypes[ii+3] <- "uido-autoCov"
  trials[ii+3] <- currenttrial
  dorms[ii+3] <- uidoAutoCovDorm
  autoCovs[ii+3] <- uidoAutoCovcov
  
  ii <- ii+4
  currenttrial <- currenttrial+1 
}