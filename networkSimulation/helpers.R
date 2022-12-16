generate.network = function(n, p) {
  
  # Calculate number of possible edges
  L <- n * (n-1)/2
  
  # Generate matrix values, sampling 0 or 1 with given probabilities
  matvals <- sample(c(0, 1), L, replace = TRUE, prob = c(1 - p,p))
  
  # From the values above, generate a symmetric matrix
  networkmat <- matrix(rep(0, n * n), ncol = n)
  mv <- 1
  for (i in 1:n) {
    for (j in 1:n) {
      if (i > j) {
        networkmat[i, j] <- matvals[mv]
        networkmat[j, i] <- matvals[mv]
        mv <- mv + 1
      }
    }
  }
  return(networkmat)
}
