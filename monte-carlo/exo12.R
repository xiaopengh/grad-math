library(dplyr)
library(patchwork)
library(microbenchmark)

set.seed(2025)

sqrt_pos <- function(z) sqrt(pmax(z, 0))

fct <- function(x, y){
  sqrt(x + y) * sin(y^4) * exp(-3*x/2 - y^2/4)
}

# helper function that generates truncated normal on [a, b[
rtnorm <- function(n, mean = 0, sd = 1, a = 0, b = Inf){
  Fa <- pnorm(a, mean, sd)
  Fb <- pnorm(b, mean, sd)
  u <- runif(n, Fa, Fb)
  qnorm(u, mean, sd)
}

dtnorm <- function(x, mean = 0, sd = 1, a = 0, b = Inf){
  Fa <- pnorm(a, mean, sd); Fb <- pnorm(b, mean, sd)
  dnorm(x, mean = mean, sd = sd) / (Fb - Fa)
}

rtexp <- function(n, rate = 1, a = 0, b = Inf){
  Fa <- pexp(a, rate = rate)
  Fb <- pexp(b, rate = rate)
  u <- runif(n, Fa, Fb)
  qexp(u, rate = rate)
}

dtexp <- function(x, rate = 1, a = 0, b = Inf){
  Fa <- pexp(a, rate = rate); Fb <- pexp(b, rate = rate)
  dexp(x, rate = rate) / (Fb - Fa)
}

mc_estimate <- function(N){
  
  # t0 <- proc.time()[3]
  ## x ~ Exp(3/2), y ~ N(0, sqrt(2)) truncated to [2, +inf[
  x <- rexp(N, rate = 1.5)
  y <- rtnorm(N, 0, sqrt(2), a = 2)
  
  ## proposal g(x, y) = (3/2)*exp(-3x/2) * 1/sqrt(2*pi * 2) * exp(-y^2/4) / (1 - pnorm(2, 0, sqrt(2)))
  vals <- sqrt_pos(x + y) * sin(y^4) * (y >= 2) * (x <= 5) * (2/3) * sqrt(4*pi) * (1 - pnorm(2, 0, sqrt(2)))
  
  # t1 <- proc.time()[3]
  est <- mean(vals)
  se <- sd(vals) / sqrt(N)
  
  list(est = est, se = se) # time_sec = as.numeric(t1-t0)
}

is_estimate <- function(N, rate = 1.5){
  # The optimal practical options for proposal densities are
  # truncated Normal and truncated exponential
  x <- rtexp(N, rate = rate, a = 0, b = 5)
  y <- rtnorm(N, sd = sqrt(2), a = 2)
  vals <- fct(x, y) / (dtexp(x, rate = rate, a = 0, b = 5) * dtnorm(y, sd = sqrt(2), a = 2))
  
  est <- mean(vals)
  se <- sd(vals) / sqrt(N)
  
  list(est = est, se = se)
}


# Test step
N_values <- seq(log(500), log(2e6), length.out = 30) |> exp() |> round() |> unique()
mc_results <- lapply(N_values, function(N) {
  result <- mc_estimate(N)
  
  ## Sortie
  data.frame(
    N = N,
    est = result$est,
    se = result$se
    # time_sec = result$time_sec
  )
})

is_results <- lapply(N_values, function(N){
  result <- is_estimate(N)
  
  ## Sortie
  data.frame(
    N = N,
    est = result$est,
    se = result$se
  )
})

mc_results <- bind_rows(mc_results)
is_results <- bind_rows(is_results)

# Theoretically most accurate estimate
I_approx <- tail(mc_results$est, 1)

# MC Estimate plot with N increasing
plot(log(mc_results$N), mc_results$est, type = "b")
abline(h = I_approx, col = 2, lwd = 2)

# Standard error plot for MC
plot(log(mc_results$N), mc_results$se, type = "b")

# Theoretically most accurate estimate
I_approx <- tail(is_results$est, 1)

# IS Estimate plot with N increasing
plot(log(is_results$N), is_results$est, type = "b")
abline(h = I_approx, col = 2, lwd = 2)

# Standard error plot for IS and MC
plot(log(is_results$N), is_results$se, type = "b", ylim = c(0, 0.012))
points(log(mc_results$N), mc_results$se, col = "blue")


# Notes from TP class ==========================================================

rate_list <- seq(1.2, 1.6, length.out = 1000)
n <- 100000

vars <- sapply(rate_list, function(rate){
  res_is <- is_estimate(n, rate = rate)
  res_is$se
})

rate_star <- rate_list[which.min(vars)] ; rate_star

# Graphical representation of variance according to rate
plot(rate_list, vars, type = "l")
points(rate_star, min(vars), col = "red")

# Re-evaluate IS with optimal rate
is_opt_results <- lapply(N_values, function(N){
  result <- is_estimate(N, rate = rate_star)
  data.frame(
    N = N,
    est = result$est,
    se = result$se
  )
})
is_opt_results <- bind_rows(is_opt_results)

# Standard error plot for IS with optimal rate
plot(log(is_opt_results$N), is_opt_results$se, type = "b", ylim = c(0, 0.012))
points(log(mc_results$N), mc_results$se, col = "blue")
# Standard error comparison (type = "h")
plot(log(mc_results$N), mc_results$se, col = "lightgreen", type = "h", ylim = c(0, 0.012), lwd = 3)
points(log(is_opt_results$N), is_results$se, col = "blue", type = "h", lwd = 3)
points(log(is_opt_results$N), is_opt_results$se, col = "red", type = "h", lwd = 3)
legend("topright", legend = c("MC", "IS rate = 1.5", "IS optimal rate"), 
       col = c("lightgreen", "blue", "red"), lwd = 3)


