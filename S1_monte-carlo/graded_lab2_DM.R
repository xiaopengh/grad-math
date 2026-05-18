library(dplyr)

K <- 3
p_true <- 2 * pnorm(K, mean = 0, sd = 1) - 1

# TO DO at home: Importance Sampling with Exponential proposal
IS_exp <- function(N, lam, K = 3){
  f <- function(x) (1 / (sqrt(2*pi)) * exp(-x^2/2)) * (x >= K)
  g <- function(x) lam * exp(-lam * (x - K)) * (x >= K)
  
  x <- rexp(N, rate = lam) + K
  vals <- f(x) / g(x)
  
  est <- 1 - 2 * mean(vals)
  se <- 2 * sd(vals)/sqrt(N)
  
  list(est = est, se = se)
}

# choice of lam
lam_values <- seq(0.1, 6, length.out = 1000)
se_values <- sapply(lam_values, function(lam){
  result <- IS_exp(1e5, lam = lam)
  result$se
})
lam_opt <- lam_values[which.min(se_values)]
plot(lam_values, se_values, type = "l")
points(lam_opt, min(se_values), col = "red", type = "p")
legend("top", legend = paste0("Optimal lambda = ", round(lam_opt, 4)), 
       col = "red", pch = 1, bty = "n", cex = 0.5)

# Run IS with optimal lam
N_values <- seq(log(1000), log(1e7), length.out = 20) |> exp() |> round() |> unique()
IS_exp_results <- lapply(N_values, function(N){
  result <- IS_exp(N, lam = lam_opt)
  data.frame(
    N = N,
    est = result$est,
    se = result$se
  )
})
IS_exp_results <- bind_rows(IS_exp_results)
plot(log(IS_exp_results$N), IS_exp_results$est, type = "b")
abline(h = p_true, col = 2, lwd = 2)
legend("top", legend = paste0("true p = ", round(p_true, 6)), 
       col = "red", lwd = 2, bty = "n", cex = 0.8)

# TO DO at home: Importance Sampling with Exponential proposal and Antithetic Variables
IS_exp_ant <- function(N, lam, K = 3){
  f <- function(x) (1 / (sqrt(2*pi)) * exp(-x^2/2)) * (x >= K)
  g <- function(x) lam * exp(-lam * (x - K)) * (x >= K)
  
  u <- runif(N)
  x1 <- -log(u) / lam + K
  x2 <- -log(1 - u) / lam + K
  vals1 <- f(x1) / g(x1)
  vals2 <- f(x2) / g(x2)
  vals <- 0.5 * (vals1 + vals2)
  
  est <- 1 - 2 * mean(vals)
  se <- 2 * sd(vals)/sqrt(N)
  
  list(est = est, se = se)
}

# Run IS with Antithetic Variables
N_values <- seq(log(1000), log(1e7), length.out = 20) |> exp() |> round() |> unique()
IS_exp_ant_results <- lapply(N_values, function(N){
  result <- IS_exp_ant(N, lam = lam_opt)
  data.frame(
    N = N,
    est = result$est,
    se = result$se
  )
})
IS_exp_ant_results <- bind_rows(IS_exp_ant_results)
plot(log(IS_exp_ant_results$N), IS_exp_ant_results$est, type = "b")
abline(h = p_true, col = 2, lwd = 2)


# Variance comparison plot
plot(log(IS_exp_results$N), IS_exp_results$se, col = "blue", type = "b", ylim = c(0, max(IS_exp_results$se)),
     xlab = "log(N)", ylab = "Standard Error", main = "Standard Error Comparison")
points(log(IS_exp_ant_results$N), IS_exp_ant_results$se, col = "red", type = "b")
legend("top", legend = c("IS Exp", "IS Exp Antithetic"), col = c("blue", "red"), lwd = 2, cex = 0.8, bty = "n")



