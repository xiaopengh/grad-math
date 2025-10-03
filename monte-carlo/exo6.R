f <- function(x){
  return((2/pi)*sqrt(1-x^2))
}

g <- function(u){
  return(1/2)
}

M <- 4/pi

accept_reject <- function(n){
  x <- numeric(n)
  for (i in 1:n){
    accept <- FALSE
    while (!accept){
      # cat("Nouvelle itération\n")
      u <- runif(1)
      y <- runif(1, -1, 1)
      accept <- (u <= f(y)/(M*g(y)))
    }
    x[i] <- y
  }
  return(x)
}

accept_reject <- function(
    n, M, # Oversampling ratio
    f, # Target density
    g  # Proposal density
){
  x <- numeric(n)
  n_acc <- 0
  while (n_acc < n) {
    intM <- ceiling((n-n_acc)*M)
    u <- runif(intM)
    y <- runif(intM, -1, 1)
    x_idx <- which(u <= f(y)/(M*g(y)))
    x[(n_acc+1):(n_acc+length(x_idx))] <- y[x_idx]
    n_acc <- n_acc + length(x_idx)
  }
  return(x)
}

hist(accept_reject(1e4, M, f, g), breaks=50, freq=FALSE, 
     col = "lightblue", xlab="x", ylab="f(x)", 
     main="Histogramme de X simulé par la méthode du rejet")
curve(expr = f(x), from = -1, to = 1, 
      col = "red", lwd = 2, add = TRUE)


