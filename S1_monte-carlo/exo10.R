library(dplyr)

set.seed(2025)

# helper function for safe square root
sqrt_pos <- function(z) sqrt(pmax(z, 0))

# helper function that generates truncated normal on [b, +inf[
rtnorm <- function(n, mean = 0, sd = 1, b = 0){
  Fb <- pnorm(b, mean, sd)
  u <- runif(n, Fb, 1)
  qnorm(u, mean, sd)
}

mc_estimate <- function(N, method = c("a", "b", "c")){
  ## h(x, y) = sqrt(x + y) * sin(y^4) * exp(-3x/2) * exp(-y^2/4) * 1{y>=2} * 1{0<=x<=5}
  method <- match.arg(method)
  
  t0 <- proc.time()[3]
  
  if (method == "a"){
    ## x ~ Unif(0,5), y ~ N(0, sqrt(2))
    x <- runif(N, 0, 5)
    y <- rnorm(N, 0, sqrt(2))
    
    ## importance sampling 
    ## δ = int h(x) = E_g[h(x)/g(x)]
    ## proposal g(x, y) = 1/5 * 1/sqrt(2*pi * 2) * exp(-y^2/4)
    vals <- sqrt_pos(x + y) * sin(y^4) * exp(-1.5*x) * (y >= 2) * 5 * sqrt(4*pi)
    
  } else if(method == "b"){
    ## x ~ Exp(3/2), y ~ N(0, sqrt(2))
    x <- rexp(N, rate = 1.5)
    y <- rnorm(N, 0, sqrt(2))
    
    ## proposal g(x, y) = (3/2)*exp(-3x/2) * 1/sqrt(2*pi * 2) * exp(-y^2/4)
    vals <- sqrt_pos(x + y) * sin(y^4) * (y >= 2) * (x <= 5) * (2/3) * sqrt(4*pi)
    
  } else if(method == "c"){
    ## x ~ Exp(3/2), y ~ N(0, sqrt(2)) truncated to [2, +inf[
    x <- rexp(N, rate = 1.5)
    y <- rtnorm(N, 0, sqrt(2), b = 2)
    
    ## proposal g(x, y) = (3/2)*exp(-3x/2) * 1/sqrt(2*pi * 2) * exp(-y^2/4) / (1 - pnorm(2, 0, sqrt(2)))
    vals <- sqrt_pos(x + y) * sin(y^4) * (y >= 2) * (x <= 5) * (2/3) * sqrt(4*pi) * (1 - pnorm(2, 0, sqrt(2)))
  }
  
  t1 <- proc.time()[3]
  est <- mean(vals)
  se <- sd(vals)/sqrt(N)
  
  list(estimate = est, se = se, time_sec = as.numeric(t1 - t0))
  
}

library(microbenchmark)
N <- 10000000
results <- microbenchmark(
  mc_estimate(N, method = "a"),
  mc_estimate(N, method = "b"),
  mc_estimate(N, method = "c"),
  times = 100
)

print(results)
plot(results)

mc1 <- mc_estimate(N, method = "a")
mc2 <- mc_estimate(N, method = "b")
mc3 <- mc_estimate(N, method = "c")

sd1 <- mc1[["se"]]/mc1[["estimate"]] %>% abs()
sd2 <- mc2[["se"]]/mc1[["estimate"]] %>% abs()
sd3 <- mc3[["se"]]/mc1[["estimate"]] %>% abs()

cost1 <- summary(results)$median[1]
cost2 <- summary(results)$median[2]
cost3 <- summary(results)$median[3]
