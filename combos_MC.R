# Estimate probabilities by Monte Carlo.
combos_MC <- function(n, # number of slots
                      k, # number of noise events
                      reps, # number of Monte Carlo repetitions
                      cyclic = FALSE
                      ) {
  # collect results in this vector. 0 means no noise sequences in sentence,
  # 1 means at least one noise sequence.
  results <- rep(0, reps)

  for (i in 1:reps) {
    # throw noise events into sentence
    sentence <- rep(0, n)
    sentence[sample(1:n, size=k, replace=FALSE)] <- 1

    # if two adjacent noises, we have at least one noise sequence
    adjacent_noises <- 0
    if (cyclic) {
      for (j in 1:n) {
        if (j == n) {
          l <- 1
        } else {
          l <- j+1
        }
        if (sentence[j] == 1 && sentence[l] == 1) {
          adjacent_noises <- 1
        }
      }
    } else {
      for (j in 2:n) {
        if (sentence[j] == 1 && sentence[j-1] == 1) {
          adjacent_noises <- 1
        }
      }
    }

    # push to results
    results[i] <- adjacent_noises
  }

  # return result: mean over all MC repetitions
  mean(results)
}


# NOT valid. But some sort of approximation.
analytical_guess <- function(n,
                             k) {
  # result is different depending on whether n is odd or even
  if (n %% 2 != 0) {
    result <- (choose((n+1)/2, k) + choose((n-1)/2, k))/choose(n, k)
  } else {
    result <- 2*choose(n/2, k)/choose(n, k)
  }

  1 - result
}


try_it_out <- function(n,
                       reps = 1000,
                       cyclic = FALSE) {
  df <- expand.grid(k=1:n, estimate=c("MC", "analytical"), prob=NA)

  for (i in 1:nrow(df)) {
    if (df[i,]$estimate == "MC") {
      df[i,]$prob <- combos_MC(n=n, k=df[i,]$k, reps=reps, cyclic=cyclic)
    } else {
      df[i,]$prob <- analytical_guess(n=n, k=df[i,]$k)
    }
  }

  df
}


# Example usage:
#
#df <- try_it_out(n=8, reps=1000)
#g <- ggplot(df, aes(x=k, y=prob, color=estimate)) + geom_point() + geom_line()
#print(g)
