#note that this script uses the uido and dorm functions from dormUido.R in the same directory

ii = 1 #initialize counter
currenttrial = 1 #initialize trial counter

#initialize vectors for each component of each run of the loop

niter <- 10000 

noises <- vector(length = niter)
stringtypes <- vector(length = niter)
trials <- vector(length = niter)



while (currenttrial <= niter)
{
  probvec <- runif(10,min=0,max=1)
  starting <- sample(1:(length(probvec)-2), size=1) #remember, lists in R perversely start at offset 1, not 0
  
  noise <- c(probvec[starting], probvec[starting+1], probvec[starting+2])
  noiseinfo <- log2(1/noise)
  probvecinfo <- log2(1/probvec)
  totalnoise <- sum(noiseinfo)
  totalprobvecinfo <- sum(probvecinfo)
  noiseprop <- totalnoise/totalprobvecinfo

  
#Doing the same thing, but now the noise will fall in the same position in a vector that is end-heavy for information, but contains the same total info
  lopsided <- sort(probvec)

#It is important that the starting point in the vector is the same for both cases, so that each comparison of noise is a true comparison
  noise2 <- c(lopsided[starting], lopsided[starting+1], lopsided[starting+2])
  noise2info <- log2(1/noise2)
  lopsidedinfo <- log2(1/lopsided)
  totalnoise2 <- sum(noise2info)

#This next is really a sanity check: this vector contains the same probabilities as the last, so this should be equal to totalprobvecinfo
  totallopsidedinfo <- sum(lopsidedinfo)
  noise2prop <- totalnoise2/totallopsidedinfo

  
  #doing the same thing, but for a vector optimized by the uido algorithm
  
  uidoinfo <- uido(probvecinfo)
  
  totalnoise4 <- sum(c(uidoinfo[starting], uidoinfo[starting+1], uidoinfo[starting+2]))
  totaluidoinfo <- sum(uidoinfo)
  noise4prop <- totalnoise4/totaluidoinfo
  
  #update vectors for each trial
  noises[ii] <- noiseprop
  stringtypes[ii] <- "random"
  trials[ii] <- currenttrial
  
  noises[ii+1] <- noise2prop
  stringtypes[ii+1] <- "asymmetric"
  trials[ii+1] <- currenttrial
  
  noises[ii+2] <- noise4prop
  stringtypes[ii+2] <- "uido-optimized"
  trials[ii+2] <- currenttrial
  
  
  ii <- ii+3
  currenttrial <- currenttrial+1 
}

#bind vectors into a data frame and add column names
propnoise.df <- data.frame(trials,noises,stringtypes)
#colnames(propnoise.df) <- c("Trial","NoiseProp","VecType")


#Graphing

library(ggplot2)

propnoise.df$trials <- as.numeric(as.character(propnoise.df$trials))
propnoise.df$noises <- as.numeric(as.character(propnoise.df$noises))

#add a column into the dataframe recoding for losing more than 50% of the info

propnoise.df$bigLoss <- ifelse(propnoise.df$noises >= 0.5, 1, 0)

#plotting bigLoss as a binary variable along with other stuff; toggle geom_line() to see the extremes of each stringtype, toggle geom_point() to see all the data, and stat_smooth() to look at the proportion of bits lost

p <- ggplot(propnoise.df, aes(trials, noises, color=stringtypes)) + 
  labs(y = "Proportion", x = "\nTrial") + #y = "Proportion of Sentences Where the Majority of Info is Lost" y = "Proportion of Bits Lost"
  #geom_line() +
  #geom_point() +
  stat_smooth() +
  scale_color_brewer(palette = "Set1") + 
  ylim(0,1) +
#  geom_jitter(aes(trials, bigLoss, color=stringtypes), height = 0.15, shape=4) +
  stat_smooth(aes(trials, bigLoss, color=stringtypes), linetype="dotdash") +
  theme_bw() + theme(panel.border = element_blank())

ggsave(p, file = "uid-sim-both.png", width = 8.09, height = 5)