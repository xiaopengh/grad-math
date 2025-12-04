library(dplyr)
library(parallel)

# Crude Monte-Carlo with Parallelization ---------------------------------------

# Function to be executed in parallel
mc_estimate <- function(N){
  ## p = P(X < 3) for X = Sum(Q_i) i = 0...S where S ~ Poisson(3.7)
  ## s.t. Q_i ~ Weibull(shape=0.5, scale=2)
  
  pois <- rpois(N, lambda = 3.7)
  vals <- sapply(pois, function(s){
    w <- rweibull(s, shape = 0.5, scale = 2)
    1 * (sum(w) < 3)
  })
  
  est <- mean(vals)
  se <- sd(vals)/sqrt(N)
  list(est = est, se = se)
}

# Determine the number of cores to use (e.g., all but one)
num_cores <- detectCores() - 1 
# Create the cluster
cl <- makeCluster(num_cores)
# Export necessary data and functions to the cluster
# Export the 'mc_estimate' function itself.
# If 'mc_estimate' relies on other variables or functions, they must be exported.
clusterExport(cl, varlist = c("mc_estimate"))
# Load necessary packages on the cluster workers
# clusterEvalQ(cl, library(package_name)) if 'mc_estimate' uses them.

# 20 values from n = 500 to n = 2,000,000
N_values <- seq(log(500), log(2e6), length.out = 20) |> exp() |> round() |> unique()
# Use parLapply to run the simulations in parallel
mc_results <- parLapply(cl, N_values, function(N){
  # The function body remains the same
  result <- mc_estimate(N)
  data.frame(
    N = N,
    est = result$est,
    se = result$se
  )
})

# Stop the cluster
stopCluster(cl)

# Combine results (same as before)
mc_results <- bind_rows(mc_results)
# Plot results
p_last <- mc_results$est[nrow(mc_results)]
plot(log(mc_results$N), mc_results$est, type = "b")
abline(h = p_last, col = 2, lwd = 2)
plot(log(mc_results$N), mc_results$se, type = "b")

# Stratified Sampling ----------------------------------------------------------

stratified_estimate <- function(N, max_strata = 1, method = c("proportional", "optimal")){
  
  # Set up method and sampling parameters
  method <- match.arg(method)
  stopifnot(max_strata >= 1)
  lambda <- 3.7
  shape <- 0.5
  scale <- 2
  
  # The strata will be defined on the Poisson variable 
  # S = 0 .. S = max_strata, S > max_strata
  probs <- c(dpois(0:max_strata, lambda = lambda), 1 - ppois(max_strata, lambda = lambda))
  
  # Determine sample sizes per strata
  if (method == "proportional"){
    n_strata <- floor(N * probs)
  } else if (method == "optimal"){
    break
  }
  
  # Generate samples and compute estimates per strata
  val_strata <- list()
  
  # Loop over strata
  for(i in seq_along(probs)){
    n_i <- n_strata[i]
    if (i-1 == 0){
      vals <- rep(1, n_i)
    } else if (i-1 <= max_strata){
      vals <- sapply(1:n_i, function(j){
        w <- rweibull(i-1, shape = shape, scale = scale)
        1 * (sum(w) < 3)
      })
    } else {
      vals <- sapply(1:n_i, function(j){
        # Sample S until S > max_strata (Oversampling method)
        s <- rpois(ceiling(1/probs[i]), lambda = lambda)
        while (max(s) <= max_strata){
          s <- rpois(ceiling(1/probs[i]), lambda = lambda)
        }
        s <- s[s > max_strata][1]
        w <- rweibull(s, shape = shape, scale = scale)
        1 * (sum(w) < 3)
      })
    }
    
    val_strata[[i]] <- vals
  }
  
  # Combine estimates from all strata
  est_strata <- sapply(seq_along(probs), function(i){
    mean(val_strata[[i]])
  })
  var_strata <- sapply(seq_along(probs), function(i){
    var(val_strata[[i]]) / n_strata[i]
  })
  est <- sum(est_strata * probs)
  se <- sqrt(sum(var_strata * probs))
  list(est = est, se = se)
}

# Number of cores to use (all but one)
num_cores <- detectCores() - 1 
cl <- makeCluster(num_cores)
clusterExport(cl, varlist = c("stratified_estimate"))
# 20 values from n = 500 to n = 2,000,000
N_values <- seq(log(500), log(2e6), length.out = 20) |> exp() |> round() |> unique()
# Use parLapply to run the simulations in parallel
mc_results <- parLapply(cl, N_values, function(N){
  # The function body remains the same
  result <- stratified_estimate(N, max_strata = 10, method = "proportional")
  data.frame(
    N = N,
    est = result$est,
    se = result$se
  )
})

# Stop the cluster
stopCluster(cl)

# Combine results (same as before)
mc_results <- bind_rows(mc_results)
# Plot results
p_last <- mc_results$est[nrow(mc_results)]
plot(log(mc_results$N), mc_results$est, type = "b")
abline(h = p_last, col = 2, lwd = 2)
plot(log(mc_results$N), mc_results$se, type = "b")



