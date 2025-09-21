inverseF <- function(u) {
  if ((u>=0) && (u<=0.4)) return(0)
  else if ((u>0.4) && (u<=0.6)) return(5)
  else if ((u>0.6) && (u<=0.9)) return(6)
  else if ((u>0.9) && (u<=1)) return(7)
  else return(8)
}

# inverseF_vec <- Vectorize(inverseF)

# vectorized version of inverseF
inverseF_vec <- function(u) {
  x <- numeric(length(u))
  x[u <= 0.4] <- 0
  x[u > 0.4 & u <= 0.6] <- 5
  x[u > 0.6 & u <= 0.9] <- 6
  x[u > 0.9 & u <= 1] <- 7
  x[u > 1] <- 8
  return(x)
}

n <- as.integer(1e4)

u <- runif(n)

# x <- sample(5:8, size=n, replace=TRUE, prob=c(0.4, 0.2, 0.3, 0.1))

# x <- sapply(u, inverseF)

x <- inverseF_vec(u)

hist(u, breaks = seq(0, 1, by=0.1), freq=FALSE,
     xlab="u", ylab="f(x)", main="Histogramme de X")

barplot(table(x)/n, ylim=c(0,0.5),
        xlab="x", ylab="count", 
        col = "red", main="Histogramme de X")

################################################################################
# exo2.1
lambda <- 2

x_sim <- -log(1-u)/lambda
hist(x_sim, breaks = 50, freq=FALSE, 
     col = "ivory", xlab="x", ylab="f(x)", 
     main="Histogramme de X simulé par la méthode de l'inversion")
curve(expr = dexp(x, rate = lambda), from = 0, to = 4, 
      col = "blue", lwd = 2, add = TRUE) # x here is just a placeholder for curve

p <- ppoints(length(x_sim))
th <- qexp(p, rate = lambda)

# qq plot of x_sim 
qqplot(x_sim, th, 
       xlab = "Quantiles de X simulé par la méthode de l'inversion", 
       ylab = "Quantiles théoriques de X par qexp", 
       main = "QQ plot de X simulé par la méthode de l'inversion et rexp"
       )
abline(0, 1, col = "blue", lty = 2, lwd = 2)


# exo2.2
lambda <- 1.5
n_param <- 10
n <- as.integer(1e4 * n_param)
u <- runif(n)
x <- -log(1-u)/lambda ; x <- matrix(x, ncol = n_param, byrow = TRUE)
s <- rowSums(x) # s = x_1 + ... + x_10 should follow a Gamma(10, 1.5)

hist(s, breaks = 50, freq=FALSE, 
     col = "lightgreen", xlab="s", ylab="f(s)", 
     main="Histogramme de S = somme de 10 exponentielles")
curve(expr = dgamma(x, shape = n_param, rate = lambda), from = 0, to = 15,
      col = "red", lwd = 2, add = TRUE)

# qq plot of s
p <- ppoints(length(s))
th <- qgamma(p, shape = n_param, rate = lambda)
qqplot(s, th,
       xlab = "Quantiles de S simulé par la méthode de l'inversion",
       ylab = "Quantiles théoriques de S par qgamma",
       main = "QQ plot de S simulé par la méthode de l'inversion et rgamma"
      )
abline(0, 1, col = "blue", lty = 2, lwd = 2)


# exo2.3
# simulate_poisson <- function(n, lambda){
#   # n: number of simulations
#   # lambda: parameter of the Poisson distribution
#   v <- integer(n)
#   count <- 0
#   while (count < n){
#     x <- -log(1-runif(1))/lambda
#     k <- 1
#     while (x < 1){
#       x <- x - log(1-runif(1))/lambda
#       k <- k + 1
#     }
#     v[count + 1] <- k - 1
#     count <- count + 1
#   }
#   return(v)
# }

simulate_poisson <- function(n, lambda, t = 1) {
  v <- integer(n)
  for (i in seq_len(n)) {
    s <- 0.0
    k <- 0L
    repeat {
      s <- s + rexp(1, rate = lambda)  # add an interarrival
      if (s > t) break                  # stop when we pass t
      k <- k + 1L
    }
    v[i] <- k
  }
  v
}

lambda <- 4
n <- as.integer(1e4)
x_sim <- simulate_poisson(n, lambda)
hist(x_sim, breaks = seq(-0.5, max(x_sim)+0.5, by=1), freq=FALSE,
     col = "lightyellow", xlab="x", ylab="f(x)", 
     main=paste("Histogramme de X simulé par la méthode de Poisson (lambda =", lambda, ")"))
points(0:max(x_sim), dpois(0:max(x_sim), lambda), 
       col = "red", pch = 19)
lines(0:max(x_sim), dpois(0:max(x_sim), lambda), 
      col = "blue", lwd = 2)

# qq plot of x_sim
p <- ppoints(length(x_sim))
th <- qpois(p, lambda)
qqplot(x_sim, th,
       xlab = "Quantiles de X simulé par la méthode de Poisson",
       ylab = "Quantiles théoriques de X par qpois",
       main = "QQ plot de X simulé par la méthode de Poisson et qpois"
)
abline(0, 1, col = "blue", lty = 2, lwd = 2)


