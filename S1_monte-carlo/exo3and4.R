## -------------------------
## Exercise 3 — Box–Muller
## -------------------------

# Cartesian Box–Muller generator of N(0,1)
BM <- function(n) {
  k <- ceiling(n/2)
  U1 <- runif(k); U2 <- runif(k)
  R  <- sqrt(-2 * log(U1))
  T  <- 2 * pi * U2
  z  <- c(R * cos(T), R * sin(T))   # 2k variates
  z[seq_len(n)]                     # return exactly n
}

set.seed(123)
z <- BM(10000)

# Validation (graphical)
par(mfrow = c(1,2))
hist(z, breaks = 60, freq = FALSE, main = "Box–Muller N(0,1)",
     xlab = "z")
curve(dnorm(x, 0, 1), add = TRUE, lwd = 2)
qqnorm(z, main = "QQ-plot vs N(0,1)")
qqline(z, lwd = 2)
par(mfrow = c(1,1))


## ---------------------------------------
## Exercise 4 — Simulation of Gaussian vectors
## X ~ N(mu, Sigma) with
## mu = (1, 2)^T,  Sigma = [[4,3],[3,9]]
## ---------------------------------------

mu    <- c(1, 2)
Sigma <- matrix(c(4,3,3,9), nrow = 2, byrow = TRUE)

# Simple mvn sampler via Cholesky
rmvnorm2 <- function(n, mu, Sigma) {
  A <- chol(Sigma)                 # Sigma = t(A) %*% A
  Z <- matrix(rnorm(n*length(mu)), ncol = length(mu))
  sweep(Z %*% A, 2, mu, `+`)
}

set.seed(123)
n  <- 10000
X  <- rmvnorm2(n, mu, Sigma)
X1 <- X[,1]; X2 <- X[,2]

# Quick checks for (X1, X2)
par(mfrow = c(1,3))
plot(X1, X2, pch = 16, cex = .4, main = "Scatter: simulated (X1, X2)")
hist(X1, breaks = 50, freq = FALSE, main = "X1 vs N(1, 4)", xlab = "X1")
curve(dnorm(x, mean = 1, sd = 2), add = TRUE, lwd = 2)
hist(X2, breaks = 50, freq = FALSE, main = "X2 vs N(2, 9)", xlab = "X2")
curve(dnorm(x, mean = 2, sd = 3), add = TRUE, lwd = 2)
par(mfrow = c(1,1))

# 2) Distribution of S = X1 + X2
# Mean(S) = 1+2 = 3
# Var(S)  = Var(X1)+Var(X2)+2*Cov(X1,X2) = 4 + 9 + 2*3 = 19
S <- X1 + X2
par(mfrow = c(1,2))
hist(S, breaks = 60, freq = FALSE, main = "S = X1 + X2",
     xlab = "S")
curve(dnorm(x, mean = 3, sd = sqrt(19)), add = TRUE, lwd = 2)
qqnorm(S, main = "QQ-plot: S vs N(3, 19)")
qqline(S, lwd = 2)
par(mfrow = c(1,1))

# Numeric sanity checks (optional)
c(mean_Z = mean(z), sd_Z = sd(z),
  mean_S = mean(S), var_S = var(S))
