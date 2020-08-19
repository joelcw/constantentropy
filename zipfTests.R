library(zipfR)
library(zoo)
library(ggplot2)
library(lme4)

Brown.probs <- Brown.tfl$f/N(Brown.tfl) #vector of word probabilities derived from the word freqs of the Brown corpus

Brown.bits <- log2(1/Brown.probs)



#Tests for Normality in small samples from Zipf distribution in Brown corpus

niter <- 1000 
ii <- 1
pvalue.raw <- vector(length = niter) #proportion of total info in the sentence destroyed by noise
pvalue.rollmean <- vector(length = niter)


while (ii <= niter)
{
  
  foo <- sample(Brown.bits, 10, replace = T, prob = Brown.probs)
  rolling <- rollmean(foo,2)
  
  #shapiro-wilk test for normality
  pvalue.raw[ii] <- shapiro.test(foo)$p.value
  pvalue.rollmean[ii] <- shapiro.test(rolling)$p.value
  
  ii <- ii+1
  
}

#bind vectors into a data frame and add column names
tests.df <- data.frame(pvalue.raw,pvalue.rollmean)


#Is p.value > various alphas for raw array and for its rolling mean with window 2

tests.df$alpha5raw <- ifelse(tests.df$pvalue.raw >= 0.5, 1, 0)
tests.df$alpha5rolling <- ifelse(tests.df$pvalue.rollmean >= 0.5, 1, 0)

tests.df$alpha05raw <- ifelse(tests.df$pvalue.raw >= 0.05, 1, 0)
tests.df$alpha05rolling <- ifelse(tests.df$pvalue.rollmean >= 0.05, 1, 0)

tests.df$alpha2raw <- ifelse(tests.df$pvalue.raw >= 0.2, 1, 0)
tests.df$alpha2rolling <- ifelse(tests.df$pvalue.rollmean >= 0.2, 1, 0)


#Same thing, but for samples of 10 sequential words
niter <- 1000 
ii <- 1
pvalue.raw <- vector(length = niter) #proportion of total info in the sentence destroyed by noise
pvalue.rollmean <- vector(length = niter)


while (ii <= niter)
{
  
  starting <- sample(1:(length(Brown.bits)-9), size=1)
  
  foo <- Brown.bits[starting:(starting+9)]
  rolling <- rollmean(foo,2)
  
  #shapiro-wilk test for normality
  pvalue.raw[ii] <- shapiro.test(foo)$p.value
  pvalue.rollmean[ii] <- shapiro.test(rolling)$p.value
  
  ii <- ii+1
  
}

#bind vectors into a data frame and add column names
testsSeq.df <- data.frame(pvalue.raw,pvalue.rollmean)


#Is p.value > various alphas for raw array and for its rolling mean with window 2

testsSeq.df$alpha5raw <- ifelse(testsSeq.df$pvalue.raw >= 0.5, 1, 0)
testsSeq.df$alpha5rolling <- ifelse(testsSeq.df$pvalue.rollmean >= 0.5, 1, 0)

testsSeq.df$alpha05raw <- ifelse(testsSeq.df$pvalue.raw >= 0.05, 1, 0)
testsSeq.df$alpha05rolling <- ifelse(testsSeq.df$pvalue.rollmean >= 0.05, 1, 0)

testsSeq.df$alpha2raw <- ifelse(testsSeq.df$pvalue.raw >= 0.2, 1, 0)
testsSeq.df$alpha2rolling <- ifelse(testsSeq.df$pvalue.rollmean >= 0.2, 1, 0)



###Same thing with Moby Dick from project gutenberg

moby.bits <- scan(file="~/CurrentLx/infoTheory/localCailScripts/mobyDick.bits.txt", what=numeric())
ggplot(data.frame("bits" = moby.bits)) + geom_density(aes(x=bits))

moby.probs <- 1/(2^moby.bits)

jacket.bits <- scan(file="~/CurrentLx/infoTheory/localCailScripts/whiteJacket.bits.txt", what=numeric())
ggplot(data.frame("bits" = moby.bits)) + geom_density(aes(x=bits))

jacket.probs <- 1/(2^jacket.bits)

moby.bits <- c(moby.bits,jacket.bits)
moby.probs <- c(moby.probs,jacket.probs)

niter <- 1000 
ii <- 1
pvalue.raw <- vector(length = niter) #proportion of total info in the sentence destroyed by noise
pvalue.rollmean <- vector(length = niter)


while (ii <= niter)
{
  
  foo <- sample(moby.bits, 10, replace = T, prob = moby.probs)
  rolling <- rollmean(foo,2)
  
  #shapiro-wilk test for normality
  pvalue.raw[ii] <- shapiro.test(foo)$p.value
  pvalue.rollmean[ii] <- shapiro.test(rolling)$p.value
  
  ii <- ii+1
  
}

#bind vectors into a data frame and add column names
tests.df <- data.frame(pvalue.raw,pvalue.rollmean)


#Is p.value > various alphas for raw array and for its rolling mean with window 2

tests.df$alpha5raw <- ifelse(tests.df$pvalue.raw >= 0.5, 1, 0)
tests.df$alpha5rolling <- ifelse(tests.df$pvalue.rollmean >= 0.5, 1, 0)

tests.df$alpha05raw <- ifelse(tests.df$pvalue.raw >= 0.05, 1, 0)
tests.df$alpha05rolling <- ifelse(tests.df$pvalue.rollmean >= 0.05, 1, 0)

tests.df$alpha2raw <- ifelse(tests.df$pvalue.raw >= 0.2, 1, 0)
tests.df$alpha2rolling <- ifelse(tests.df$pvalue.rollmean >= 0.2, 1, 0)


#Same thing, but for samples of 10 sequential words
niter <- 1000 
ii <- 1
pvalue.raw <- vector(length = niter) #proportion of total info in the sentence destroyed by noise
pvalue.rollmean <- vector(length = niter)


while (ii <= niter)
{
  
  starting <- sample(1:(length(moby.bits)-9), size=1)
  
  foo <- moby.bits[starting:(starting+9)]
  rolling <- rollmean(foo,2)
  
  #shapiro-wilk test for normality
  pvalue.raw[ii] <- shapiro.test(foo)$p.value
  pvalue.rollmean[ii] <- shapiro.test(rolling)$p.value
  
  ii <- ii+1
  
}

#bind vectors into a data frame and add column names
testsSeq.df <- data.frame(pvalue.raw,pvalue.rollmean)


#Is p.value > various alphas for raw array and for its rolling mean with window 2

testsSeq.df$alpha5raw <- ifelse(testsSeq.df$pvalue.raw >= 0.5, 1, 0)
testsSeq.df$alpha5rolling <- ifelse(testsSeq.df$pvalue.rollmean >= 0.5, 1, 0)

testsSeq.df$alpha05raw <- ifelse(testsSeq.df$pvalue.raw >= 0.05, 1, 0)
testsSeq.df$alpha05rolling <- ifelse(testsSeq.df$pvalue.rollmean >= 0.05, 1, 0)

testsSeq.df$alpha2raw <- ifelse(testsSeq.df$pvalue.raw >= 0.2, 1, 0)
testsSeq.df$alpha2rolling <- ifelse(testsSeq.df$pvalue.rollmean >= 0.2, 1, 0)