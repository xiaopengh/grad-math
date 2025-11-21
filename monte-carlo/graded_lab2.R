library(dplyr)

p_true <- 2 * pnorm(3, mean = 0, sd = 1) - 1

MC_unif <- function(N, K = 3){
  f <- function(x) 2*K / (sqrt(2*pi)) * exp(-x^2/2)
  x <- runif(N, min = -K, max = K)
  vals <- f(x)
  
  est <- mean(vals)
  se <- sd(vals)/sqrt(N)
  
  list(est = est, se = se)
}

N_values <- seq(log(1000), log(1e7), length.out = 20) |> exp() |> round() |> unique()
MC_unif_results <- lapply(N_values, function(N){
  result <- MC_unif(N)
  
  data.frame(
    N = N,
    est = result$est,
    se = result$se
  )
})

MC_unif_results <- bind_rows(MC_unif_results)
plot(log(MC_unif_results$N), MC_unif_results$est, type = "b", ylim = c(0.995, 1))
abline(h = p_true, col = 2, lwd = 2)

# EXO2 =========================================================================
# Crude Monte-Carlo
MC_norm2 <- function(N){
  x <- rnorm(N, mean = 0, sd = 1)
  vals <- (x >= 3) * 1
  
  est <- 1 - 2 * mean(vals)
  se <- 2 * sd(vals)/sqrt(N)
  
  list(est = est, se = se)
}

# Run MC
N_values <- seq(log(1000), log(1e7), length.out = 20) |> exp() |> round() |> unique()
MC_norm2_results <- lapply(N_values, function(N){
  result <- MC_norm2(N)
  
  data.frame(
    N = N,
    est = result$est,
    se = result$se
  )
})

MC_norm2_results <- bind_rows(MC_norm2_results)
plot(log(MC_norm2_results$N), MC_norm2_results$est, type = "b", ylim = c(0.995, 1))
abline(h = p_true, col = 2, lwd = 2)

# Importance Sampling with Normal proposal N(mu, 1)
IS_norm <- function(N, mu, K = 3){
  f <- function(x) (1 / (sqrt(2*pi)) * exp(-x^2/2)) * (x >= K)
  g <- function(x) 1 / (sqrt(2*pi)) * exp(-(x - mu)^2/2)
  
  x <- rnorm(N, mean = mu, sd = 1)
  vals <- f(x) / g(x)
  
  est <- 1 - 2 * mean(vals)
  se <- 2 * sd(vals)/sqrt(N)
  
  list(est = est, se = se)
}

# Choice of mu
mu_values <- seq(-1, 5, length.out = 1000)
se_values <- sapply(mu_values, function(mu){
  result <- IS_norm(1e5, mu = mu)
  result$se
})
mu_opt <- mu_values[which.min(se_values)]
plot(mu_values, se_values, type = "l")
points(mu_opt, min(se_values), col = "red", type = "p")

# Run IS with optimal mu
N_values <- seq(log(1000), log(1e7), length.out = 20) |> exp() |> round() |> unique()
IS_norm_results <- lapply(N_values, function(N){
  result <- IS_norm(N, mu = mu_opt)
  data.frame(
    N = N,
    est = result$est,
    se = result$se
  )
})

IS_norm_results <- bind_rows(IS_norm_results)
plot(log(IS_norm_results$N), IS_norm_results$est, type = "b", ylim = c(0.9972, 0.9975))
abline(h = p_true, col = 2, lwd = 2)

# Importance Sampling with Antithetic Variables
IS_norm_ant <- function(N, mu, K = 3){
  f <- function(x) (1 / (sqrt(2*pi)) * exp(-x^2/2)) * (x >= K)
  g <- function(x) 1 / (sqrt(2*pi)) * exp(-(x - mu)^2/2)
  
  x1 <- rnorm(N, mean = mu, sd = 1)
  x2 <- 2 * mu - x1
  
  vals1 <- f(x1) / g(x1)
  vals2 <- f(x2) / g(x2)
  
  vals <- 0.5 * (vals1 + vals2)
  
  est <- 1 - 2 * mean(vals)
  se <- 2 * sd(vals)/sqrt(N)
  
  list(est = est, se = se)
}

# Run IS with Antithetic Variables
N_values <- seq(log(1000), log(1e7), length.out = 20) |> exp() |> round() |> unique()
IS_norm_ant_results <- lapply(N_values, function(N){
  result <- IS_norm_ant(N, mu = mu_opt)
  data.frame(
    N = N,
    est = result$est,
    se = result$se
  )
})

IS_norm_ant_results <- bind_rows(IS_norm_ant_results)
plot(log(IS_norm_ant_results$N), IS_norm_ant_results$est, type = "b", ylim = c(0.9972, 0.9975))
abline(h = p_true, col = 2, lwd = 2)
# ==============================================================================

# EXO3 =========================================================================
I_true <- 2 # 2/lam^2

# Crude Monte Carlo
MC_exp_int <- function(N){
  x <- rexp(N)
  vals <- x^2
  
  est <- mean(vals)
  se <- sd(vals)/sqrt(N)
  
  list(est = est, se = se)
}

N_values <- seq(log(1000), log(1e7), length.out = 20) |> exp() |> round() |> unique()
results <- lapply(N_values, function(N){
  result <- MC_exp_int(N)
  
  data.frame(
    N = N,
    est = result$est,
    se = result$se
  )
})
results <- bind_rows(results)
plot(log(results$N), results$est, type = "b")
abline(h = I_true, col = 2, lwd = 2)


# Control variables
CV_exp_int <- function(N, train_ratio = 0.1){
  N_train <- ceiling(N * train_ratio)

  x <- rexp(N)

  # Training data
  x_train <- x[1:N_train]
  vals_train <- x_train^2

  # Estimate optimal coefficient c using training data
  # c* = Cov(X^2, X) / Var(X)
  c_opt <- cov(vals_train, x_train) / var(x_train)

  # Test data
  x_test <- x[(N_train + 1):N]
  vals_test <- x_test^2

  # Control variable estimator
  # θ̂_CV = mean(X^2) - c*(mean(X) - E[X])
  # E[X] = 1 for Exp(1)
  est_cv <- mean(vals_test) - c_opt * (mean(x_test) - 1)

  # Standard error from residuals
  residuals <- vals_test - c_opt * x_test
  se <- sd(residuals) / sqrt(length(x_test))

  list(est = est_cv, se = se, c_opt = c_opt)
}

N_values <- seq(log(1000), log(1e7), length.out = 20) |> exp() |> round() |> unique()
results <- lapply(N_values, function(N){
  result <- CV_exp_int(N)
  
  data.frame(
    N = N,
    est = result$est,
    se = result$se
  )
})
results <- bind_rows(results)
plot(log(results$N), results$est, type = "b")
abline(h = I_true, col = 2, lwd = 2)



