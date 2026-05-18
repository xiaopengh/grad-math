# EXO1
inverseF <- function(u){
  x <- numeric(n)
  x[u <= 0.4] <- 5
  x[(u <= 0.6) & (u > 0.4)] <- 6
  x[(u <= 0.9) & (u > 0.6)] <- 7
  x[u > 0.9] <- 8
  x
}

n <- as.integer(1e5)

u <- runif(n)

x <- inverseF(u)

hist(u, breaks = seq(0, 1, by = 0.1), freq = FALSE)
curve(dunif(x), from = 0, to = 1, add = TRUE,
      col = "red", lwd = 2)

barplot(table(x)/n, ylim = c(0, 0.5))

# ==============================================================================

# EXO2
# Exp distribution
sim_exp <- function(n, lam = 1){
  n <- as.integer(n)
  u <- runif(n)
  inverseF <- function(u) -log(1-u)/lam
  x <- inverseF(u)
  x
}

n <- 1e5

x <- sim_exp(n, lam = 2)

hist(x, freq = FALSE, breaks = 50)
curve(dexp(x, rate = lam), from = 0, to = 5, add = TRUE,
      col = "red", lwd = 2)

p <- ppoints(n)
th <- qexp(p, rate = lam)

qqplot(th, x, col = adjustcolor(col = "black", alpha.f = 0.1))
abline(a = 0, b = 1, col = "blue", lwd = 2)

# Gamma distribution
sim_gamma <- function(n, n_param, lam = 1){
  N <- as.integer(n*n_param)
  rowSums(matrix(sim_exp(N, lam = lam), ncol = n_param))
}

s <- sim_gamma(n, n_param = 10, lam = 2)

hist(s, freq = FALSE, breaks = 50)
curve(dgamma(x, shape = 10, rate = 2), from = 0, to = 12, add = TRUE,
      col = "red", lwd = 2)

p <- ppoints(n)
th <- qgamma(p, shape = 10, rate = 2)

qqplot(th, s, col = adjustcolor(col = "black", alpha.f = 0.1))
abline(a = 0, b = 1, col = "blue", lwd = 2)

# Poisson distribution
sim_poisson <- function(n, lam = 1){
  n <- as.integer(n)
  v <- integer(n)
  for (i in 1:n){
    s <- 0.0
    k <- 0L
    repeat{
      s = s + rexp(1, lam)
      if (s > 1) break
      k = k + 1
    }
    v[i] <- k
  }
  v
}

pois <- sim_poisson(n, 1)
hist(pois, freq = F, breaks = seq(-0.5, max(pois)+0.5, by = 1))
points(0:max(pois), dpois(0:max(pois), lam = 1))
lines(0:max(pois), dpois(0:max(pois), lam = 1))

# ============================================================================== 

# accept_reject <- function(n, M){
#   x <- numeric(n)
#   n_acc <- 0
#   while (n_acc < n){
#     intM <- ceiling((n - n_acc)*M)
#     u <- runif(intM)
#     y <- rdist(intM, params)
#     y_idx <- which(f(y)/M*g(y) <= u)
#     if (length(x_idx) > 0){
#       x[n_acc+1 : n_acc+length(x_idx)] <- y[y_idx]
#     }
#     n_acc <- n_acc + length(x_idx)
#   }
#   x[1:n]
# }


