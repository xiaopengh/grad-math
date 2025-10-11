tr_norm <- function(n, b, mu, sd){
  # Generate n samples from a N(mu, sd^2) truncated to [b, +Inf)
  f <- function(y, b, mu, sd){
    # Density of the target distribution
    phi <- pnorm((mu-b)/sd)
    return(ifelse(y >= b, dnorm(y, mu, sd)/phi, 0)) # ask if this is necessary
  }
  
  M <- 1/pnorm((mu - b)/sd)
  
  x <- numeric(n)
  n_acc <- 0
  while (n_acc < n) {
    intM <- ceiling((n-n_acc)*M)
    u <- runif(intM)
    y <- rnorm(intM, mu, sd)
    x_idx <- which(u <= f(y, b, mu, sd)/(M*dnorm(y, mu, sd)))
    if (length(x_idx) > 0) {
      x[(n_acc+1):(n_acc+length(x_idx))] <- y[x_idx]
    }
    n_acc <- n_acc + length(x_idx)
  }
  return(x[1:n])
}

tr_norm_2 <- function(n, b, mu, sd){
  f <- function(y, b, mu, sd){
    # Density of target distribution
    phi <- pnorm((mu-b)/sd)
    return(ifelse(y >= b, dnorm(y, mu, sd)/phi, 0))
  }
  
  lam <- ifelse(b > mu, (b-mu)/(2*sd^2), (b-mu+sqrt((b-mu)^2+4*sd^2))/(2*sd^2))
  M <- 1/(sd*sqrt(2*pi) *pnorm((mu-b)/sd) * lam) * exp((lam*sd)^2/2 + lam*(mu-b))
  
  g <- function(y, b, mu, sd){
    # Density of the proposal distribution
    return(ifelse(y >= b, lam*exp(-lam*(y-b)), 0))
  }
  x <- numeric(0)
  n_acc <- 0
  while (n_acc < n){
    intM <- ceiling((n-n_acc)*M)
    u <- runif(intM)
    y <- rexp(intM, lam) + b
    x_idx <- which(u <= f(y, b, mu, sd)/(M*g(y, b, mu, sd)))
    if (length(x_idx) != 0){
      x[(n_acc+1):(n_acc+length(x_idx))] <- y[x_idx]
    }
    n_acc <- n_acc + length(x_idx)
  }
  return(x[1:n])
}

# Generate samples
samples <- tr_norm(as.integer(1e4), 2, 0, 2)
# Plot histogram of samples
hist(samples, breaks = 50, probability = TRUE, col = "lightblue",
     main = "Truncated Normal Samples Normal Proposal", xlab = "Value")
# Overlay the theoretical density
curve(dnorm(x, 0, 2)/pnorm((0 - 2)/2), from = 2, to = 8, col = "red", add = TRUE)

# Generate samples 
samples <- tr_norm_2(as.integer(1e4), 2, 0, 2)
# Plot histogram of samples
hist(samples, breaks = 50, probability = TRUE, col = "lightgreen",
     main = "Exponential proposal b > mu")
# Overlay the theoretical density
curve(dnorm(x, 0, 2)/pnorm((0 - 2)/2), from = 2, to = 8, col = "red", add = TRUE)

# Generate samples
samples <- tr_norm_2(as.integer(1e4), 1, 2, 2)
# Plot histogram of samples
hist(samples, breaks = 50, probability = TRUE, col = "lightgreen",
     main = "Exponential proposal b < mu")
# Overlay the theoretical density
curve(dnorm(x, 2, 2)/pnorm((2 - 1)/2), from = 0, to = 8, col = "red", add = TRUE)

library(microbenchmark)
n <- as.integer(1.5e5)
results <- microbenchmark(
  method1 = tr_norm(n, 2, 0, 2),
  method2 = tr_norm_2(n, 2, 0, 2),
  times = 100
)

print(results)
plot(results)