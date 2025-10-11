library(microbenchmark)

estimate_pi_1 <- function(n){
  f <- function(x){
    return(ifelse(abs(x)<=1, sqrt(1-x^2), 0))
  }
  u <- runif(as.integer(n), -1, 1)
  hx <- f(u) * 4
  return(c(sum(hx)/n, var(hx)))
}

estimate_pi_2 <- function(n){
  u1 <- runif(as.integer(n))
  u2 <- runif(as.integer(n))
  hx <- 4 * ifelse(u1^2 + u2^2 <= 1, 1, 0)
  est <- sum(hx)/n
  return(c(est, var(hx)))
}

# Compare execution times
n <- 1.5e5
results <- microbenchmark(
  method1 = estimate_pi_1(n)[1],
  method2 = estimate_pi_2(n)[1],
  times = 100
)

mc1 <- estimate_pi_1(n)
mc2 <- estimate_pi_2(n)

var1 <- mc1[2]
var2 <- mc2[2]

cost1 <- summary(results)$median[1]
cost2 <- summary(results)$median[2]

ratio <- (var1 * cost1) / (var2 * cost2)

summary(results)
plot(results)