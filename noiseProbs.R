
library(plotly)

#This function takes k noise events in a string of n words, and returns the probability that 2 or more noise events occur in a row.
#Note: for the prob that all events are in a row, use: ((n - k + 1)*factorial(k)*factorial(n-k))/factorial(n)
probSeq <- function(n,k)
{

    prob <- ((n - k + 1)*factorial(k)*factorial(n-k))/factorial(n)
#Note this is the same as (factorial(k)*(n - k + 1)) divided by permutation of k out of n , 
  return(prob)
}

n <- seq(1,50,by=1)
k <- seq(1,50,by=1)

probs.df <- data.frame(matrix(nr = 2500, nc=3))
colnames(probs.df) <- c("Noises","StringLengths","Probability")

#probs.df$p <- probSeq(k,n)
#create data frame with n for every k that is less than or equal to n, and then iterate probSeq over it

ii <- 1
for (number in n)
{
  for (numb in k)
  {
    if (numb <= number)
    {
    
      probs.df$Noises[ii] <- numb
      probs.df$StringLengths[ii] <- number
      ii <- ii+1
      
    }
    
  }
   
}

probs.df$Noises <- as.numeric(as.character(probs.df$Noises))
probs.df$StringLengths <- as.numeric(as.character(probs.df$StringLengths))

ii <- 1
while (ii <= 2500)
{
  probs.df$Probability[ii] <- probSeq(probs.df$Noises[ii],probs.df$StringLengths[ii])
  ii <- ii+1
}