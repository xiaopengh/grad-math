library(dplyr)
library(patchwork)

set.seed(2025)

# helper function that generates truncated normal on [a, b[
rtnorm <- function(n, mean = 0, sd = 1, a = 0, b = Inf){
  Fa <- pnorm(a, mean, sd)
  Fb <- pnorm(b, mean, sd)
  u <- runif(n, Fa, Fb)
  qnorm(u, mean, sd)
}


mc_estimate <- function(N, method = c("unif", "tnorm")){
  # Crude monte carlo method
  method <- match.arg(method)
  
  t0 <- proc.time()[3]
  
  if (method == "unif"){
    u <- runif(N, 0, 2)
    vals <- 2 * exp(-u^2)
  }
  
  if (method == "tnorm"){
    sd <- 1/sqrt(2)
    x <- rtnorm(N, sd = sd, a = -2, b = 2)
    vals <- (pnorm(2, sd = sd) - pnorm(-2, sd = sd)) * sqrt(pi) * (x >= 0) 
  }
  
  t1 <- proc.time()[3]
  est <- mean(vals)
  se <- sd(vals)/sqrt(N)
  
  list(est = est, se = se, time_sec = as.numeric(t1-t0))
}

av_estimate <- function(N, method = c("unif", "tnorm")){
  
  method = match.arg(method)
  
  if (method == "tnorm"){
    # Proposal density dnorm(sd)/(Fb - Fa)
    sd <- 1/sqrt(2)
    h <- (pnorm(2, sd = sd) - pnorm(-2, sd = sd)) * sqrt(pi)
    x <- rtnorm(N, sd = sd, a = -2, b = 2)
    vals <- h * ((x >= 0) + (x <= 0))
  }
  
  if (method == "unif"){
    # Proposal density 1/2
    u <- runif(N, 0, 2)
    vals <- 2 * (exp(-u^2) + exp(-(2 - u)^2))
  }
    
  est <- mean(vals) / 2
  se <- sd(vals)/sqrt(N)
  
  list(est = est, se = se)
}

N_values <- seq(log(500), log(2e6), length.out = 20) |> exp() |> round() |> unique()

mc_results <- lapply(N_values, function(N){
  result <- mc_estimate(N, method = "unif")
  
  data.frame(
    N = N,
    est = result$est,
    se = result$se
  )
})

av_results <- lapply(N_values, function(N){
  result <- av_estimate(N, method = "unif")
  
  data.frame(
    N = N,
    est = result$est,
    se = result$se
  )
})

mc_results <- bind_rows(mc_results)
av_results <- bind_rows(av_results)

I_true <- (pnorm(2, sd = sqrt(1/2)) - pnorm(0, sd = sqrt(1/2))) * sqrt(pi)

# Graphical validation for crude monte-carlo method 
plot(log(mc_results$N), mc_results$est, type = "b")
abline(h = I_true, col = 2, lwd = 2)

# Standard error plot for crude monte-carlo
plot(log(av_results$N), av_results$se, type = "b")

# Graphical validation for antithetic variable method 
plot(log(av_results$N), av_results$est, type = "b")
abline(h = I_true, col = 2, lwd = 2)

# Standard error plot for antithetic variable
plot(log(av_results$N), av_results$se, type = "b")

# Standard error comparison
plot(log(mc_results$N), mc_results$se, type = "b", ylim = c(0, 0.02))
points(log(av_results$N), av_results$se, col = "blue")

# Standard error comparison (type = "h")
plot(log(mc_results$N), mc_results$se, type = "h", col = "grey", ylim = c(0, 0.02), lwd = 3, 
     main = "Standard Error Comparison", ylab = "Standard Error", xlab = "log(N)")
points(log(av_results$N), av_results$se, type = "h", col = "blue", lwd = 3)
legend("topright", legend = c("Crude MC", "Antithetic Var."), col = c("grey", "blue"), lwd = 3)


sv_estimate <- function(N, allocation = c("equal", "optimal")){
  # Stratified sampling method
  K <- 10
  
  allocation <- match.arg(allocation)
  if (allocation == "equal"){
    n_per_stratum <- rep(floor(N / K), K)
  }
  if (allocation == "optimal"){
    # Preliminary run to estimate variances in each stratum
  }
  est_strata <- numeric(K)
  var_strata <- numeric(K)
  for (k in 1:K){
    u <- runif(n_per_stratum[k], (k - 1)/K, k/K)
    x <- 2 * u
    vals_stratum <- 2 * exp(-x^2)
    
    est_strata[k] <- mean(vals_stratum)
    var_strata[k] <- var(vals_stratum) / n_per_stratum[k]
  }
  
  # The choice of n_per_stratum doesn't affect the probability
  est <- sum(est_strata) / K
  se <- sqrt(sum(var_strata)) / K
  
  list(est = est, se = se)
}

sv_results <- lapply(N_values, function(N){
  result <- sv_estimate(N, allocation = "equal")
  
  data.frame(
    N = N,
    est = result$est,
    se = result$se
  )
})
sv_results <- bind_rows(sv_results)
# Graphical validation for stratified sampling method
plot(log(sv_results$N), sv_results$est, type = "b")
abline(h = I_true, col = 2, lwd = 2)

# ==============================================================================
# Compare standard errors
plot(log(mc_results$N), mc_results$se, col = "grey", type = "h", ylim = c(0, 0.02), lwd = 3,
     main = "Standard Error Comparison", ylab = "Standard Error", xlab = "log(N)")
points(log(av_results$N), av_results$se, col = "blue", type = "h", lwd = 3)
points(log(sv_results$N), sv_results$se, col = "red", type = "h", lwd = 3)
legend("topright", legend = c("Crude MC", "Antithetic Var.", "Stratified Sampling"), 
       col = c("grey", "blue", "red"), lwd = 3)






