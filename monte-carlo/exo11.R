library(dplyr)

# function to estimate
# int_50^infty 1 / (pi*(1 + x^2)) dx

rperato <- function(n, a = 50, k = 1){
  # invF(x) <- a * (1-x)^(-1/k)
  x <- runif(n)
  a * (1-x)^(-1/k)
}

dperato <- function(x, a = 50, k = 1){
  # Perato distribution density
  k * a^k / x^(k+1)
}

# use dcauchy(x, 0, 1)

mc_estimate <- function(N){
  
  x <- rcauchy(N, 0, 1)
  vals <- (1 * (x >= 50))
  
  est <- mean(vals)
  se <- sd(vals)/sqrt(N)
  
  list(est = est, se = se)
}

is_estimate <- function(N){
  
  x <- rperato(N)
  vals <- (dcauchy(x, 0, 1) / dperato(x)) * (x >= 50)
  
  est <- mean(vals)
  se <- sd(vals)/sqrt(N)
  
  list(est = est, se = se)
}

# estim_p_cauchy <- function(n){
#   x <- rcauchy(n, 0, 1)
#   (1 * (x >= 50)) |> mean()
# }
# 
# estim_p_perato <- function(n){
#   x <- rperato(n)
#   ((dcauchy(x, 0, 1) / dperato(x)) * (x >= 50)) |> mean()
# }

# 20 values from n = 500 to n = 2,000,000
N_values <- seq(log(500), log(2e6), length.out = 20) |> exp() |> round() 

results <- matrix(0, nrow = length(N_values) * 2, ncol = 4)
colnames(results) <- c("N", "est", "se", "type")

for (i in seq_along(N_values)) {
  N <- N_values[i]
  IS <- is_estimate(N) ; MC <- mc_estimate(N)
  results[2*i-1, ] <- c(N, unlist(MC), "MC")
  results[2*i, ] <- c(N, unlist(IS), "IS")
}

# ---------------------------------------------------------
# Plot results
# ---------------------------------------------------------
p_true <- 1 - pcauchy(50, 0, 1)

p1 <- {
  plot(N_values, 
       results[results[, "type"] == "IS", "est"], 
       type="b", log="x",
       xlab="n", ylab="Estimate",
       main="Importance Sampling (Pareto)")
  abline(h = p_true, col = 2, lwd = 2)
}

p2 <- {
  plot(N_values, 
       results[results[, "type"] == "MC", "est"], 
       type="b", log="x",
       xlab="n", ylab="Estimate",
       main="Crude Monte Carlo (Cauchy)")
  abline(h = p_true, col = 2, lwd = 2)  
}

p3 <- {
  plot(N_values,
       results[results[, "type"] == "IS", "se"],
       type = "b", log = "x", ylim = c(0, 0.004),
       xlab = "n", ylab = "Standard Error")
  points(N_values,
         results[results[, "type"] == "MC", "se"],
         type = "b", col = "blue", pch = 3)
}


