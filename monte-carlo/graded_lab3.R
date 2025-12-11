library(dplyr)

# PART1: Crude Monte-Carlo to estimate the integral of f(x) over [-1, 1]--------
# It is indeed a density function since int_-1^1 f(x) dx = 1
f <- function(x){
  return(ifelse(abs(x)<=1, 3*(1-x^2)/4, 0))
}

p_true <- 1
mc_estimate <- function(N){
  x <- runif(N, -1, 1)
  vals <- f(x)
  
  est <- mean(vals) * 2
  se <- sd(vals)/sqrt(N) * 2
  
  list(est = est, se = se)
}

N_values <- seq(log(1000), log(1e6), length.out = 20) |> exp() |> round() |> unique()
mc_results <- lapply(N_values, function(N){
  result <- mc_estimate(N)
  
  data.frame(
    N = N,
    est = result$est,
    se = result$se
  )
})
mc_results <- bind_rows(mc_results)
plot(log(mc_results$N), mc_results$est, type = "b")
abline(h = p_true, col = 2, lwd = 2)

# CDF of f
F_f <- function(x){
  if (x < -1){
    return(0)
  } else if (x >= -1 & x <= 1){
    return(0.5 + (3*x)/4 - (x^3)/4)
  } else {
    return(1)
  }
}

plot(seq(-2, 2, length.out = 200), sapply(seq(-2, 2, length.out = 200), F_f), type = "l")

# It is possible to sample from f using inverse transform sampling
# Because F_f is strictly increasing over [-1, 1], we can find its inverse

# PART2: Accept-Reject Sampling for X ~ f --------------------------------------
rtraget_accept_reject <- function(N){
  # We use a uniform proposal over [-1, 1] and oversampling
  # The accept reject algorithm's procedure is as follows:
  # 1. Sample Y ~ g, where g is the proposal density (here uniform over [-1, 1])
  # 2. Sample U ~ Uniform(0, 1)
  # 3. Accept Y as a sample from f if U <= f(Y) / (M * g(Y)), where M is a constant such that f(x) <= M * g(x) for all x
  # 4. Repeat until we have N accepted samples
  # The theoretical accept rate is 1/M
  # The formula of acceptance-rejection sampling gives M = max_x f(x) / g(x)
  f <- function(x){
    return(ifelse(abs(x)<=1, 3*(1-x^2)/4, 0))
  }
  M <- 1.5 # since max_x f(x) = 3/4 and g(x) = 1/2 over [-1, 1]
  N_accept <- 0
  samples <- numeric(N)
  while (N_accept < N){
    u <- runif(ceiling((N - N_accept) * M))
    y <- runif(ceiling((N - N_accept) * M), -1, 1)
    idx_accept <- which(u <=  (f(y) / (M * 0.5)))
    int_M <- min(length(idx_accept), N - N_accept)
    samples[(N_accept+1) : (N_accept + int_M)] <- y[idx_accept[1 : int_M]]
    if (sum(is.na(samples)) != 0){
      print(is.na(samples))
      break
    }
    N_accept <- N_accept + int_M
  }
  samples
}

n <- 1e5
samples <- rtraget_accept_reject(n)

# Histogram of samples from accept-reject
hist(samples, breaks = 50, probability = TRUE)
curve(f(x), from = -1, to = 1, add = TRUE, col = "red", lwd = 2)

# Inverse CDF of f
F_f_inv <- function(u){
  if (u < 0 || u > 1){
    stop("u must be in [0, 1]")
  }
  if (u == 0){
    return(-1)
  } else if (u == 1){
    return(1)
  }
  f <- function(x) F_f(x) - u
  result <- uniroot(f, interval = c(-1, 1))
  return(result$root)
}
F_f_inv <- Vectorize(F_f_inv)

# QQplot to validate the sampling
probs <- ppoints(n)
q_theo <- sapply(probs, F_f_inv)
qqplot(q_theo, samples,
       main = "QQplot of Theoretical vs Accept-Reject Samples",
       cex = 0.1, pch = 1)

# PART3: Monte-Carlo Estimation of E[X^2] where X ~ f --------------------------
I_true <- 0.2 # True value of E[X^2] where X ~ f
MC <- function(N){
  alpha <- 0.05
  x <- rtraget_accept_reject(N)
  vals <- x^2
  
  est <- mean(vals)
  se <- sd(vals)/sqrt(N)
  confint <- est + c(-1, 1) * qnorm(1 - alpha/2) * se
  
  list(est = est, se = se, confint = confint)
}

# Run MC
N_values <- seq(log(1000), log(1e6), length.out = 20) |> exp() |> round() |> unique()
mc_results <- lapply(N_values, function(N){
  result <- MC(N)
  tibble(
    N = N,
    est = result$est,
    se = result$se,
    confint_lower = result$confint[1],
    confint_upper = result$confint[2]
  )
})
mc_results <- bind_rows(mc_results)
plot(log(mc_results$N), mc_results$est, type = "b")
abline(h = I_true, col = 2, lwd = 2)
# Add confidence intervals
arrows(log(mc_results$N), mc_results$confint_lower,
       log(mc_results$N), mc_results$confint_upper,
       angle = 90, code = 3, length = 0.05, col = "blue")


# PART4: Variance Reduction using Control Variates -----------------------------
# First note that (let X ~ f) 
# E[X^(2k)] = 3/2 ( \frac{1}{2k+1} - \frac{1}{2k+3} )
# E(X^2) = 1/5, E(X^4) = 3/35, E(X^6) = 1/21
# Theoretically, the optimal b is given by Cov(h(X), h_0(X)) / Var(x_0(X))
CV1 <- function(N){
  alpha <- 0.05
  train_rate <- 0.1
  N_train <- ceiling(N * train_rate)
  N_est <- N - N_train
  x_train <- rtraget_accept_reject(N_train)
  x_est <- rtraget_accept_reject(N_est)
  # Theoretical optimal coefficient b = Cov(X^2, X^4) / Var(X^4)
  b <- cov(x_train^2, x_train^4) / var(x_train^4)
  vals <- x_est^2 - b * (x_est^4 - 3/35)
  
  est <- mean(vals)
  se <- sd(vals)/sqrt(N_est)
  confint <- est + c(-1, 1) * qnorm(1 - alpha/2) * se
  
  list(est = est, se = se, confint = confint, b = b)
}

CV2 <- function(N){
  alpha <- 0.05
  train_rate <- 0.1
  N_train <- ceiling(N * train_rate)
  N_est <- N - N_train
  x_train <- rtraget_accept_reject(N_train)
  x_est <- rtraget_accept_reject(N_est)
  # Theoretical optimal coefficient b = Cov(X^2, X^6) / Var(X^6)
  b <- cov(x_train^2, x_train^6) / var(x_train^6)
  vals <- x_est^2 - b * (x_est^6 - 1/21)
  
  est <- mean(vals)
  se <- sd(vals)/sqrt(N_est)
  confint <- est + c(-1, 1) * qnorm(1 - alpha/2) * se
  
  list(est = est, se = se, confint = confint, b = b)
}

# Run CV1 and CV2
# Run CV1
N_values <- seq(log(1000), log(1e6), length.out = 10) |> exp() |> round() |> unique()
cv1_results <- lapply(N_values, function(N){
  result <- CV1(N)
  tibble(
    N = N,
    est = result$est,
    se = result$se,
    confint_lower = result$confint[1],
    confint_upper = result$confint[2],
    b = result$b
  )
})
cv1_results <- bind_rows(cv1_results)
plot(log(cv1_results$N), cv1_results$est, 
     type = "b", col = "black",
     ylim = c(min(cv1_results$confint_lower, cv2_results$confint_lower),
              max(cv1_results$confint_upper, cv2_results$confint_upper)))
abline(h = I_true, col = 2, lwd = 2)
# Add confidence intervals
arrows(log(cv1_results$N), cv1_results$confint_lower,
       log(cv1_results$N), cv1_results$confint_upper,
       angle = 90, code = 3, length = 0.05, col = "green")

# Run CV2
cv2_results <- lapply(N_values, function(N){
  result <- CV2(N)
  tibble(
    N = N,
    est = result$est,
    se = result$se,
    confint_lower = result$confint[1],
    confint_upper = result$confint[2],
    b = result$b
  )
})
cv2_results <- bind_rows(cv2_results)
points(log(cv2_results$N), cv2_results$est, type = "b", col = "blue")
# Add confidence intervals
arrows(log(cv2_results$N), cv2_results$confint_lower,
       log(cv2_results$N), cv2_results$confint_upper,
       angle = 90, code = 3, length = 0.05, col = "orange")
legend("topright", legend = c("CV1", "CV2"), col = c("green", "orange"), lwd = 2)



