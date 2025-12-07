# Plot estimates---------------------------------------------------------------- 
# Purpose: plot mc_results(estimates) w.r.t N(Number of samples) with confidence intervals

plot_estimates <- function(mc_results, 
                           p_true = mc_results$est[nrow(mc_results)]){ # Default: last estimate
  plot(log(mc_results$N), mc_results$est, type = "b",
       ylab = "Estimate", xlab = "log(N)",
       main = "Monte Carlo Estimates with Confidence Intervals")
  points(log(mc_results$N), mc_results$conf.lower,
         col = "dodgerblue", type = "l") # pch 24: triangle point up
  points(log(mc_results$N), mc_results$conf.upper, 
         col = "dodgerblue", type = "l") # pch 25: triangle point down
  abline(h = p_true, col = 2, lwd = 2)
  cat("Last 5 estimates with CI:\n", 
      paste0(round(tail(mc_results$est, 5), 6), 
             " [", 
             round(tail(mc_results$conf.lower, 5), 6), ", ", 
             round(tail(mc_results$conf.upper, 5), 6), "]", 
             collapse = "\n "))
  
}

# Plot standard errors----------------------------------------------------------
# Purpose: plot mc_results(standard errors) w.r.t N(Number of samples)
plot_standard_errors <- function(mc_results){
  plot(log(mc_results$N), mc_results$se, type = "b",
       ylab = "Standard Error", xlab = "log(N)",
       main = "Monte Carlo Standard Errors")
  print(paste("Last 5 standard errors: ", paste(round(tail(mc_results$se, 5), 6), collapse = ", ")))
}
