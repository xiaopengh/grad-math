dinvgamma <- function(x, alpha, beta){
  (beta^alpha / gamma(alpha)) * x^(-alpha-1) * exp(-beta/x) * (x>=0)
}

pinvgamma <- function(q, alpha, beta){
  1 - pgamma(q, alpha, 1/beta)
}

qinvgamma <- function(p, alpha, beta){
  1 / qgamma(1-p, alpha, beta)
}

rinvgamma1 <- function(n, alpha, beta){
  1 / rgamma(n, alpha, scale = 1/beta)
}

rinvgamma2 <- function(n, alpha, beta){
  u <- runif(n)
  qinvgamma(u, alpha, beta)
}

alpha = 3
beta = 2
n = 10000

r1 = rinvgamma1(n, alpha, beta)
r2 = rinvgamma2(n, alpha, beta)

hist(r1, breaks = 50, freq = FALSE)
curve(dinvgamma(x, alpha, beta), from = 0, to = 3, add = TRUE)

hist(r2, breaks = 50, freq = FALSE)
curve(dinvgamma(x, alpha, beta), from = 0, to = 3, add = TRUE)

qqplot(r1, r2)
abline(a = 0, b = 1, col = "blue", lwd = 2)

mc_estimate <- function(n, alpha, beta, b){
  y <- invgamma::rinvgamma(n, shape = alpha, scale = beta)
  const <- invgamma::pinvgamma(b, alpha, scale = beta) - invgamma::pinvgamma(0, alpha, scale = beta)
  h <- function(y) (1-y)^2 * (y >= 0) * (y <= b) / const
  hy <- h(y)
  c(mean(hy), var(hy)/n)
}

mc_estimate_trunc <- function(n, alpha, beta, b){
  y <- rinvgamma_trunc1(n, alpha, beta)
  h <- function(y) (1-y)^2 * (y >= 0) * (y <= b)
  hy <- h(y)
  c(mean(hy), var(hy)/n)
}

n <- 10000
N <- 100
m <- numeric(N)
v <- numeric(N)

for (i in 1:N){
  c(m[i], v[i]) <- mc_estimate(n, 3, 2, 5)
}




