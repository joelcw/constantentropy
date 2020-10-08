
library(plotly)

#This function takes k noise events in a string of n words, and returns the probability that 2 or more noise events occur in a row.

probSeq <- function(k,n)
{
  ii = 2
  prob = 0
  
  while (ii <= k)
  {
    
    prob <- prob + (((factorial(k)^2)*factorial(n-k))/(ii*factorial(n-1)))
    ii <- ii+1
    
  }
  
  return(prob)
}

n <- seq(1,50,by=1)
k <- seq(1,50,by=1)

probs.df <- data.frame(k,n)
#probs.df$p <- probSeq(k,n)
#create data frame with n for every k and then iterate probSeq over it