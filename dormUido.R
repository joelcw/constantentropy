#computes all pairwise means of adjacent numbers of the array. Then, get the sd of the means, the DORM, which we want to minimize.

#If you set the "correct" option to TRUE, then the function adds a correction to the dorm equal to the probability that the uniquely most uniform distribution of 
#probabilities in the vector (the uido) could be arrived at by chance, i.e. the probability of arriving at 1 order out of all possible unique orders for the probabilities.
#This both penalizes shorter utterances, since it is more likely for shorter utterances that a speaker might have achieved UIDO by chance.
#(Also, shorter utterances will be less resistant to noise anyway, as per Shannon's theorem and Plotkin and Nowak.) E.g., a one-word utterance will have perfect UIDO
#by definition, because there's no alteernative order. It also penalizes repetitions of a single word, as in ASD stereotyped language.
#Uses: library(combinat), permn(), unique()

library(zoo)

dorm <- function(logvec,correct=FALSE)
{
  
  #In the case of a single-item vector, there is no rolling mean with window 2, and no standard deviation of means, so we just set the dorm to 0 in that case.
  if (length(logvec) == 1)
  {
    logvecdorm = 0
  }

  #The usual case, where the vector has more than 1 element...
  else
  {
    #takes rolling means for pairs of numbers, which we will use to compute the deviation of rolling means (dorm)
    means <- rollmean(logvec,2)
    
    logvecdorm <- sd(means)
      
  }
  
  if (correct==TRUE)
    {
    library(combinat)
    
    #generates all possible permutations of logvec, then applies unique() which returns a list of only the unique ones (in case there are doubles)
    uniquePerms <- unique(permn(logvec))
    
    #Takes the number of unique permutations, and calculates the probability of arriving at a specific one by chance, which then becomes the penalty we add to the dorm.
    penalty <- 1/(length(uniquePerms))
    
    logvecdorm <- logvecdorm+penalty
    }
  
  
  return(logvecdorm)
}

#dormoldschool; I could put in the dorm function I hand-coded instead of using rollmean(), but do we need it? It might be slightly faster, but whatever.
#Maybe this is also useful for Alice to see what the rolling mean actually is.

dormoldschool <- function(logvec)
{
kk = 1
pairmeanlist = array()
#save all means of adjacent numbers into a list of means
while ((kk+1) <= length(logvec)) #remember, lists in R perversely start at offset 1, not 0
{
  pairmean = (logvec[kk]+logvec[kk+1])/2
  pairmeanlist[kk] <- pairmean
  kk = kk+1
}
return(sd(pairmeanlist))
}


#The uido function permutes the order of a vector of probabilites till it finds the one that most uniformly distributes the information contents derived from
#the probabilities. You can then use the dorm() function defined above to get a heuristic for uniformity of the most uniform distribution, and compare it to 
#some observed distribution.

#The input should be a vector of probabilities. It won't be normally dist, but the function converts it to info content, log2(1/p) , which
#should make it normally dist enough to compute standard deviations as usual, especially after the rolling mean is taken (Central Limit Theorem).
uido <- function(probvec)
{
  infovec <- log2(1/probvec)
  
  #remember, lists in R perversely start at offset 1, not 0
  midpoint <- round((length(infovec))/2) #remember, lists in R perversely start at offset 1, not 0
  
  #I've modified this to do decreasing because it migth be better for head-initial langs
  newlist <- sort(infovec,decreasing=T)
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
  
  #Gets the sd of the rolling pairwise means, the DORM, which we want to minimize, using the dorm function.
  
  prevsd <- dorm(newlist)
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
    
    currentsd <- dorm(hyplist)
    
    
    #if the swap helped, then save that version of the list, and start the process over again, starting the counter at 1 again
    if (currentsd < prevsd)
    { 
      print(newlist) #debug
      #print((prevsd-currentsd)) #debug
      print(sort(infovec)) #debug
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
