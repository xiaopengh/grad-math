f <- function(x1, x2){
  # Density of the target distribution
  return(ifelse(x1^2 + x2^2 <= 1, 1/pi, 0))
}

g <- function(u1, u2){
  # Density of the proposal distribution
  return(ifelse(abs(u1) <= 1 & abs(u2) <= 1, 1/4, 0))
}

M <- 4/pi

accept_reject_nonvec <- function(n, M, # Oversampling ratio
                                 f, # Target density
                                 g  # Proposal density
){
  x1 <- numeric(n)
  x2 <- numeric(n)
  for (i in 1:n){
    accept <- FALSE
    while (!accept){
      u <- runif(1)
      y1 <- runif(1, -1, 1) ; y2 <- runif(1, -1, 1)
      accept <- (u <= f(y1, y2)/(M*g(y1, y2)))
    }
    x1[i] <- y1
    x2[i] <- y2
  }
  return(cbind(x1, x2))
}

accept_reject_vec <- function(n, M, # Oversampling ratio
                              f, # Target density
                              g  # Proposal density
){
  x1 <- numeric(n)
  x2 <- numeric(n)
  n_acc <- 0
  while (n_acc < n) {
    intM <- ceiling((n-n_acc)*M)
    u <- runif(intM)
    y1 <- runif(intM, -1, 1) ; y2 <- runif(intM, -1, 1)
    x_idx <- which(u <= f(y1, y2)/(M*g(y1, y2)))
    x1[(n_acc+1):(n_acc+length(x_idx))] <- y1[x_idx]
    x2[(n_acc+1):(n_acc+length(x_idx))] <- y2[x_idx]
    n_acc <- n_acc + length(x_idx)
  }
  return(cbind(x1, x2))
}

accept_reject_ve

# Generate samples
samples <- accept_reject_vec(10000, M, f, g)

# Create 2D histogram bins
nbins <- 30
x_bins <- seq(-1, 1, length.out = nbins + 1)
y_bins <- seq(-1, 1, length.out = nbins + 1)

# Count points in each bin
counts <- table(cut(samples[,1], breaks = x_bins),
                cut(samples[,2], breaks = y_bins))

# Plot as image
image(x_bins[-length(x_bins)], y_bins[-length(y_bins)], 
      counts, 
      col = topo.colors(100),
      xlab = "x1", ylab = "x2",
      main = "2D Histogram")