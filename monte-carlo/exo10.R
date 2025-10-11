# u <- uniform; n <- normal; e <- exponential;
# un_estim <- X sampled with uniform distribution, Y sampled with normal distribution

un_estim <- function(n){
  x <- runif(n, 0, 5)
  y <- rnorm(n, 0, sqrt(2))
  h <- function(x, y){
    val = sqrt(2*pi*2)*5*sqrt(x+y)*sin(y^4)*exp(-3*x/2) # added abs for x+y
    return(ifelse(y >= 2, val, 0))
  }
  hx <- h(x, y)
  return(c(sum(hx)/n, var(hx)))
}

en_estim <- function(n){
  x <- rexp(n, rate = 3/2)
  y <- rnorm(n , 0 , sqrt(2))
  h <- function(x, y){
    val <- ifelse((x <= 5) & (y >= 2),
                  (2/3)*sqrt(4*pi*(x+y))*sin(y^4), 0)
    return(val)
  }
  hx <- h(x, y)
  return(c(sum(hx)/n, var(hx)))
}