# import dataset
df <- read.csv("data/temperature.csv", sep = ";", dec = ",",
               row.names = 1)

# Convert string numbers to numeric values
df[, !names(df) %in% c("Area")] <- lapply(df[, !names(df) %in% c("Area")], 
                                          function(x) as.numeric(as.character(x)))
# df[, -ncol(df)] <- lapply(df[, -ncol(df)], function(x) as.numeric(as.character(x)))

X <- as.matrix(df[, -ncol(df)])

# Centered matrix Y
center_mat <- function(X){
  X - rep(1, nrow(X)) %*% t(colMeans(X))
}

Y <- center_mat(X)

# Variance matrix
var_mat <- function(X){
  Y <- center_mat(X)
  (t(Y) %*% Y)/nrow(X)
}

V <- var_mat(X)

# Calculate eigenvalue results of V
result <- eigen(V)

trV <- sum(result$values)
lam1 <- result$values[1] ; lam2 <- result$values[2]
tau1 <- lam1/trV ; tau2 <- lam2/trV

# Proportion of variance explained
tau <- numeric(ncol(V))
for (i in 1:ncol(V)){
  tau[i] <- result$values[i] / trV
}

plot(tau, type = "b")

# Matrix whose 2 first columns are 2 principle axis
P <- Y %*% result$vectors

{ # Visualization of 2 principle axis
  n <- 15
  plot(P[1:n, 1:2], xlab = "1st axis", ylab = "2nd axis")
  text(P[1:n, 1:2], labels = row.names(P)[1:n], pos = 3)
}

qlt <- function(X){
  Y <- center_mat(X)
  V <- var_mat(X)
  result <- eigen(V)
  P <- Y %*% result$vectors
  
}



     