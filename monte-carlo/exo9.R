library(microbenchmark)

estimate_pi_1 <- function(n){
  f <- function(x){
    return(ifelse(abs(x)<=1, sqrt(1-x^2), 0))
  }
  
  u <- runif(as.integer(n), -1, 1)
  
  return(sum(f(u))/n * 4)
}

estimate_pi_2 <- function(n){
  u1 <- runif(as.integer(n))
  u2 <- runif(as.integer(n))
  est <- 4 * sum(ifelse(u1^2 + u2^2 <= 1, 1, 0))/n
  return(est)
}

# Compare execution times
n <- 1.5e5
results <- microbenchmark(
  method1 = estimate_pi_1(n),
  method2 = estimate_pi_2(n),
  times = 100
)

print(results)
plot(results)