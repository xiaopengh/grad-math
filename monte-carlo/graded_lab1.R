## EXO1
dinvgamma <- function(x, alpha, beta){
  # Corrected from Gamma(alpha) to gamma(alpha)
  (beta^alpha/gamma(alpha)) * x^(-alpha-1) * exp(- beta/x) * (x > 0)
}

pinvgamma <- function(q, alpha, beta){
  1 - pgamma(1/q, alpha, scale = 1/beta)
}


qinvgamma <- function(p, alpha, beta){
  qgamma(1-p, alpha, scale = 1/beta)^(-1)
}

rinvgamma1 <- function(n, alpha, beta){
  rgamma(n, alpha, scale = 1/beta)^(-1)
}

rinvgamma2 <- function(n, alpha, beta){
  u <- runif(n)
  qinvgamma(u, alpha, beta)
}

n <- 10^4 ; alpha <- 3 ; beta <- 2 # If n <- 1e4 then it will be processed as float

x1 <- rinvgamma1(n, alpha, beta)

x2 <- rinvgamma2(n, alpha, beta)

qqplot(x1, x2, xlim = c(0, 10), ylim = c(0, 10),
       xlab = "quantile by rgamma", ylab = "quantile by inverse F")
abline(a = 0, b = 1, lwd = 2, col = "blue")

p <- ppoints(n)
th <- qinvgamma(p, alpha, beta)

qqplot(th, x1, xlim = c(0, 10), ylim = c(0, 10),
       xlab = "theo quantile", ylab = "quantile by rgamma")
abline(a = 0, b = 1, lwd = 2, col = "blue")

qqplot(th, x2, xlim = c(0, 10), ylim = c(0, 10),
       xlab = "theo quantile", ylab = "quantile by inverse F")
abline(a = 0, b = 1, lwd = 2, col = "blue")

library("microbenchmark")
result = microbenchmark(rinvgamma1(n, alpha, beta), 
                        rinvgamma2(n, alpha, beta),
                        times = 100)
print(result)
plot(result)


## EXO2
# useful function
dinvgamma_trunc <- function(x, alpha, beta, b){
  # In our previous code there was an error here because we use Gamma() in dinvgamma
  dinvgamma(x, alpha, beta)/(pinvgamma(b,alpha,beta) - pinvgamma(0,alpha,beta)) * (x <= b) 
}

qinvgamma_trunc <- function(p, alpha, beta, b){
  F_0 <- pinvgamma(0, alpha, beta)
  F_b <- pinvgamma(b, alpha, beta)
  prob <- F_0 + p * (F_b - F_0)
  qinvgamma(prob, alpha, beta)
}

# inverse transform sampling
rinvgamma_trunc1 = function(n, alpha, beta, b){
  u <- runif(n)
  qinvgamma_trunc(u, alpha, beta, b)
}

# validation by histogram
b <- 5
x1 <- rinvgamma_trunc1(n, alpha, beta, b)
hist(x1, breaks = 50, freq = F, xlim = c(0 , 9))
curve(dinvgamma_trunc(x, alpha, beta, b), from = 0, to = 5, add = T, col = "red", lwd = 2)

# accept-reject by invgamma proposal
rinvgamma_trunc2 <- function(n, alpha, beta, b){
  M <- (pinvgamma(b,alpha,beta) - pinvgamma(0,alpha,beta))^(-1) # we jast looked at sup(f/g) and this quantity is what left
  n_acc <- 0
  x <- numeric(n)
  while (n_acc < n){
    n_trials <- ceiling(M*(n - n_acc))
    u <- runif(n_trials)
    y <- rinvgamma1(n_trials , alpha, beta)
    y_idx <- which(u <= dinvgamma_trunc(y, alpha, beta, b)/(M*dinvgamma(y, alpha, beta)))
    if (length(y_idx) != 0){
      x[(n_acc+1) : (n_acc+length(y_idx))] <-  y[y_idx]
    }
    n_acc <- n_acc + length(y_idx)
  }
  x
}

# validation by qqplot
x1 <- rinvgamma_trunc1(n, alpha, beta, b)
x2 <- rinvgamma_trunc2(n, alpha, beta, b)
qqplot(x1, x2, xlim = c(0, 6), ylim = c(0, 6), 
       xlab = "Inverse transform sampling",
       ylab = "Accept - Reject sampling")
abline(a = 0, b = 1, col = "blue", lwd = 2)

p <- ppoints(n)
th <- qinvgamma_trunc(p, alpha, beta, b)

qqplot(th, x1, xlim = c(0, 6), ylim = c(0, 6), 
       xlab = "Theoretical quantile",
       ylab = "Inverse transform sampling")
abline(a = 0, b = 1, col = "blue", lwd = 2)

qqplot(th, x2, xlim = c(0, 6), ylim = c(0, 6), 
       xlab = "Theoretical quantile",
       ylab = "Accept - Reject sampling")
abline(a = 0, b = 1, col = "blue", lwd = 2)

# accept-reject by uniform proposal
rinvgamma_trunc3 <- function(n, alpha, beta, b){
  const <- b/(pinvgamma(b,alpha,beta) - pinvgamma(0,alpha,beta)) * beta^alpha/gamma(alpha)
  M <- ifelse(b <= beta/(alpha+1), 
              const * b^(-alpha-1) * exp(-beta/b),
              const * (beta/(alpha+1))^(-alpha-1) * exp(-alpha-1))
  n_acc <- 0
  x <- numeric(n)
  while (n_acc < n){
    n_trials <- ceiling(M*(n - n_acc))
    u <- runif(n_trials)
    y <- runif(n_trials, min = 0, max = b)
    y_idx <- which(u <= dinvgamma_trunc(y, alpha, beta, b)/M * b)
    if (length(y_idx) != 0){
      x[(n_acc+1) : (n_acc+length(y_idx))] <-  y[y_idx]
    }
    n_acc <- n_acc + length(y_idx)
  }
  x
}

# validation by qqplot and histogram 
x3 <- rinvgamma_trunc3(n, alpha, beta, b)
qqplot(th, x2, xlim = c(0, 6), ylim = c(0, 6), 
       xlab = "Theoretical quantile",
       ylab = "Accept - Reject sampling uniform proposal")
abline(a = 0, b = 1, col = "blue", lwd = 2)

hist(x3, breaks = 50, freq = F, xlim = c(0 , 9))
curve(dinvgamma_trunc(x, alpha, beta, b), from = 0, to = 5, add = T, col = "red", lwd = 2)

# time efficiency 
{result = microbenchmark(
  "Inverse Gamma Proposal" = rinvgamma_trunc2(n, alpha, beta, b = 5), 
  "Uniform Proposal" = rinvgamma_trunc3(n, alpha, beta, b = 5),
  times = 100)
print(result)
plot(result, main = "truncated on [0, 5]")}

{result = microbenchmark(
  "Inverse Gamma Proposal" = rinvgamma_trunc2(n, alpha, beta, b = 0.5), 
  "Uniform Proposal" = rinvgamma_trunc3(n, alpha, beta, b = 0.5),
  times = 100)
print(result)
plot(result, main = "truncated on [0, 0.5]")}


## EXO3
mc_estimate <- function(n, alpha, beta, b){
  y <- rinvgamma1(n, alpha, beta)
  const <- pinvgamma(b, alpha, beta) - pinvgamma(0, alpha, beta)
  h <- function(y){(1-y^2) * (y>=0) * (y<=b) / const}
  hy <- h(y)
  c(mean(hy), var(hy)/n)
}

mc_estimate_trunc <- function(n, alpha ,beta, b){
   y <- rinvgamma_trunc1(n, alpha, beta, b)
   h <- function(y) (1-y^2) * (y>=0) * (y<=b)
   hy <- h(y)
   c(mean(hy), var(hy)/n)
}

eval_var <- function(n_sim, n, alpha, beta, b){
  res_mc <- matrix(NA, nrow = n_sim, ncol = 2)
  res_mc_trunc <- matrix(NA, nrow = n_sim, ncol = 2)
  for (i in 1:n_sim){
    res_mc[i, ] <- mc_estimate(n, alpha, beta, b)
    res_mc_trunc[i, ] <- mc_estimate_trunc(n, alpha, beta, b)
  }
  boxplot(list("inv gamma" = res_mc[,1],
               "trunc inv gamma" = res_mc_trunc[,1]),
          main = paste0("Comparison of two Monte Carlo estimators (b=", b, ")"),
          ylab = "Value of the estimator")
  boxplot(list("inv gamma" = res_mc[,2],
               "trunc inv gamma" = res_mc_trunc[,2]),
          main = paste0("Comparison of variance (b=", b, ")"),
          ylab = "Variance of the estimator")
}

{# comparison of variance and time on [0, 5]
  n = 10^4 ; alpha = 3 ; beta = 2 ; b = 5
  eval_var(100, n ,alpha, beta, b)
  result <- microbenchmark(
    "inv gamma" = mc_estimate(n, alpha ,beta, b),
    "trunc inv gamma" = mc_estimate_trunc(n, alpha ,beta, b),
    times = 100
  )
  plot(result, main = "MC estimation time comparison on [0, 5]" , xlab = FALSE)
}

{# comparison of variance and time on [0, 0.5]
  n = 10^4 ; alpha = 3 ; beta = 2 ; b = 0.5
  eval_var(100, n ,alpha, beta, b)
  result <- microbenchmark(
    "inv gamma" = mc_estimate(n, alpha ,beta, b),
    "trunc inv gamma" = mc_estimate_trunc(n, alpha ,beta, b),
    times = 100
  )
  plot(result, main = "MC estimation time comparison on [0, 0.5]" , xlab = FALSE)
}