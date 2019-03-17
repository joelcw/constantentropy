ii = 1 #initialize counter
currenttrial = 1 #initialize trial counter

#initialize vectors for each component of each run of the loop

niter <- 10000 

noises <- vector(length = niter)
stringtypes <- vector(length = niter)
trials <- vector(length = niter)

#propnoise.df <- data.frame(Trial=as.numeric(character()), NoiseProp=as.numeric(character()), VecType=character()) 


#Make sure the array that is input to this function is either normally dist, or if it's probabilities, that you've already taken log2(1/p) of the list
joelopt <- function(loglist)
{
  #remember, lists in R perversely start at offset 1, not 0
  midpoint <- round((length(loglist))/2) #remember, lists in R perversely start at offset 1, not 0
  
  newlist <- sort(loglist)
  mm = 1 #remember, lists in R perversely start at offset 1, not 0
  jj = length(newlist)
  
  while (jj >= midpoint)
  {
    #starting from both ends of the array, swaps every other pair of numbers, which gets us very close to optimized
    cup <- newlist[mm]
    newlist[mm] <- newlist[jj]
    newlist[jj] <- cup
    mm = mm+2
    jj = jj-2
  }  
    
  #computes all pairwise means of adjacent numbers of the array. Then, get the sd of the means, the DORM, which we want to minimize.
  #TODO: make this a separate function so you can call in independently of joelopt, which you should rename uido.
  kk = 1
  pairmeanlist = array()
  #save all means of adjacent numbers into a list of means
  while ((kk+1) <= length(newlist)) #remember, lists in R perversely start at offset 1, not 0
    {
      pairmean = (newlist[kk]+newlist[kk+1])/2
      pairmeanlist[kk] <- pairmean
      kk = kk+1
  }
  
  #All the following sd() calls create what we called in the grant "Deviations of Rolling Means (DORMs)".
  
  prevsd <- sd(pairmeanlist)
  currentsd <- prevsd #initialize currentsd
    
  #Now, see if swapping any pair of numbers gets us a lower sd for pairmeanlist. If any swap does, then do it, otherwise don't. Repeat till no swap helps.
  ll = 1
  while((ll+1) <= length(newlist))
    {
      #do the swap
      if ((ll-1) == 0)
      {
        hyplist <- array(c(newlist[ll+1], newlist[ll], newlist[(ll+2):length(newlist)]))
      }
      else if ((ll+1) == length(newlist))
      {
        hyplist <- array(c(newlist[1:(ll-1)], newlist[ll+1], newlist[ll]))
      }
      else
      {
        hyplist <- array(c(newlist[1:(ll-1)],newlist[ll+1],newlist[ll],newlist[(ll+2):length(newlist)]))
      }
      
      
      #computes all pairwise means of adjacent numbers of the array. Then, get the sd of the means, which we want to minimize.
      #I now see that I could have used the function "rollmean" in package "zoo", but I'm glad I did it old school here anyway.
      kk = 1
      hyppairmeanlist = array()
      #save all means of adjacent numbers into a list of means
      while ((kk+1) <= length(hyplist)) #remember, lists in R perversely start at offset 1, not 0
      {
        pairmean = (hyplist[kk]+hyplist[kk+1])/2
        hyppairmeanlist[kk] <- pairmean
        kk = kk+1
      }
      currentsd <- sd(hyppairmeanlist)
      
      
      #if the swap helped, then save that version of the list, and start the process over again, starting the counter at 1 again
      if (currentsd < prevsd)
      { 
        print(newlist) #debug
        #print((prevsd-currentsd)) #debug
        print(sort(loglist)) #debug
        newlist <- hyplist
        prevsd <- currentsd
        ll = 1
        print("we improved") #debug
      }
      else
      {ll = ll+1}
      
    }
    
  #print("and here is new dorm") #debug
  #print(prevsd)#debug
  
  return(newlist)
  
}

#Caveat: the above is all fine for the sim, because all arrays are of the same length. However, for computing DORMs in the wild, we will need to account for different
#lengths by penalizing shorter utterances, since it is more likely for shorter utterances that a speaker might have achieved UIDO by chance.
#(Also, shorter utterances will be less resistant to noise anyway, as per Shannon's theorem and Plotkin and Nowak.) E.g., a one-word utterance will have perfect UIDO
#by definition, because there's no alteernative order. So, the penalty needs to be applied for the prob of getting a perfect UIDO by chance, out of all the 
#possible *unique* orderings of word probabilities. This will penalize not just one-word utterances, but also repetitions of a single word, as in ASD stereotyped language.
#I would suggest calculating a probability of UIDO by chance, p, and then adding that to the DORM (where minimal DORM is closest to UIDO). p must be calculated based on
#all possible *unique* permutations of the starting array of word probs. TODO: Here is a function for computing and adding a penalty:





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

#Doing the same thing, but for a vector designed to be symmetrical
  symmetrical <- vector(length=10)
  symmetrical[1] <- lopsided[9]
  symmetrical[2] <- lopsided[2]
  symmetrical[3] <- lopsided[8]
  symmetrical[4] <- lopsided[4]
  symmetrical[5] <- lopsided[6]
  symmetrical[6] <- lopsided[5]
  symmetrical[7] <- lopsided[7]
  symmetrical[8] <- lopsided[3]
  symmetrical[9] <- lopsided[1]
  symmetrical[10] <- lopsided[10]
  
  noise3 <- c(symmetrical[starting], symmetrical[starting+1], symmetrical[starting+2])
  
  noise3info <- log2(1/noise3)
  
  symmetricalinfo <- log2(1/symmetrical)
  
  totalnoise3 <- sum(noise3info)
  
  #This next is really a sanity check: this vector contains the same probabilities as the last, so this should be equal to totalprobvecinfo
  totalsymmetricalinfo <- sum(symmetricalinfo)
  
  noise3prop <- totalnoise3/totalsymmetricalinfo
  
  #doing the same thing but for a vector designed to be symmetrical by the joelopt algorithm above. Note that joelopt takes vectors that have already been logged, so that's
  #why it looked a little different below.
  
  joeloptinfo <- joelopt(probvecinfo)
  
  totalnoise4 <- sum(c(joeloptinfo[starting], joeloptinfo[starting+1], joeloptinfo[starting+2]))
  
  totaljoeloptinfo <- sum(joeloptinfo)
  
  noise4prop <- totalnoise4/totaljoeloptinfo
  
  #update vectors for each trial
  noises[ii] <- noiseprop
  stringtypes[ii] <- "random"
  trials[ii] <- currenttrial
  
  noises[ii+1] <- noise2prop
  stringtypes[ii+1] <- "asymmetric"
  trials[ii+1] <- currenttrial
  
  noises[ii+2] <- noise3prop
  stringtypes[ii+2] <- "symmetric"
  trials[ii+2] <- currenttrial
  
  noises[ii+3] <- noise4prop
  stringtypes[ii+3] <- "joelopt"
  trials[ii+3] <- currenttrial
  
  
  #add rows to dataframe for the current run of the sim (THIS DIDN'T WORK BUT IM LEAVING IT IN COMMENTS TO SEE IF IT CAN IN THE FUTURE)
  #propnoise.df <- rbind(propnoise.df,c(ii,noiseprop,"random"))
  #propnoise.df <- rbind(propnoise.df,c(ii,noise2prop,"asymmetric"))
  
  ii <- ii+4
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

#plotting bigLoss as a binary variable along with other stuff; toggle geom_line() to see the extremes of each stringtype, toggle geom_point() to see all the data

p <- ggplot(propnoise.df, aes(trials, noises, color=stringtypes)) + 
  labs(y = "Proportion of Sentences Where the Majority of Info is Lost", x = "\nTrial") + 
  #geom_line() +
  #geom_point() +
  #stat_smooth() +
  scale_color_brewer(palette = "Set1") + 
  ylim(0,1) +
#  geom_jitter(aes(trials, bigLoss, color=stringtypes), height = 0.15, shape=4) +
  stat_smooth(aes(trials, bigLoss, color=stringtypes), linetype="dotdash") +
  theme_bw() + theme(panel.border = element_blank())

ggsave(p, file = "uid_sim.png", width = 8.09, height = 5)






#using google ngrams to get some estimates for sample sentences; note: "submit(ted)" is actually the sum of submitted and submitting, and Rach(a)el includes both spelling variants

sentence1 <- matrix(nc=2,nr=13,c("there","will","be","a","bid","submit(ted)","in","March","by","Christine","Joel","and","Rach(a)el",0.00177,0.00165,0.00464,0.01665,0.0000149,2.082e-05,0.0168,0.0000994,0.00426,0.00000519,0.00000552,0.0245,1.213828e-05))

sentence1 <- data.frame(sentence1)

colnames(sentence1) <- c("Word","Prob")

#orders the levels of the Word factor so that they can be plotted in the right order, i.e. not alphabetical
sentence1$Word <- ordered(sentence1$Word, levels = c("there","will","be","a","bid","submit(ted)","in","March","by","Rach(a)el", "Christine","and","Joel"))

sentence1$Prob <- as.numeric(as.character(sentence1$Prob))
sentence1$Info <- -log2(sentence1$Prob)

sentence2 <-matrix(nc=2,nr=12,c("in","March","Christine","will","be","submit(ting)","a","bid","with","Joel","and","Rach(a)el",0.0168,0.0000994,0.00000519,0.00165,0.00464,2.082e-05,0.01665,0.0000149,0.00565,0.00000552,0.0245,1.213828e-05))

sentence2 <- data.frame(sentence2)

colnames(sentence2) <- c("Word","Prob")

sentence2$Word <- ordered(sentence2$Word, levels = c("in","March","Christine","will","be","submit(ting)","a","bid","with","Joel","and","Rach(a)el"))

sentence2$Prob <- as.numeric(as.character(sentence2$Prob))
sentence2$Info <- -log2(sentence2$Prob)

library(splines)
library(MASS)
p <- ggplot(sentence1, aes(Word, Info, group=1)) + 
  labs(y = "Info Content in Bits", x = "\nWord") + 
  #geom_line() +
  geom_point() +
  stat_smooth(se=F, method= "lm", formula = y ~ ns(x,df=3)) +
  scale_color_brewer(palette = "Set1") + 
  #ylim(0,1) +
  theme_bw() + theme(panel.border = element_blank())

ggsave(p, file = "sentence1_info.png", width = 8.09, height = 5)

p <- ggplot(sentence2, aes(Word, Info, group=1)) + 
  labs(y = "Info Content in Bits ", x = "\nWord") + 
  #geom_line() +
  geom_point() +
  stat_smooth(se=F, method= "lm", formula = y ~ ns(x,df=3)) +
  scale_color_brewer(palette = "Set1") + 
  #ylim(0,1) +
  theme_bw() + theme(panel.border = element_blank())

ggsave(p, file = "sentence2_info.png", width = 8.09, height = 5)
