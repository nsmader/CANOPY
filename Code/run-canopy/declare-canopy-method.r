########################################################################
#
# SIMPLE SIMULATED ANNEALING METHOD
#   Adapted from http://www.r-bloggers.com/simulated-annealing-in-julia/
#
########################################################################

# canopy()
# Arguments:
# * cost: Function from states to the real numbers. Often called an energy function, but this algorithm works for both positive and negative costs. Lower costs are better. If the objective is expressed in "positive terms", can flip its sign to frame it as cost.
# * s0: The initial state of the system.
# * neighbor: Function from states to states. Produces what the Metropolis algorithm would call a proposal.
# * temperature: Function specifying the temperature at time i.
# * iterations: How many iterations of the algorithm should be run? This is the only termination condition.
# * keep_best: Do we return the best state visited or the last state visited? This is defaulted to true and, for now, is not offered as an option to turn off.

canopy <- function(cost,
                   s0,
                   lower,
                   upper,
                   neighbor,
                   temperature,
                   iterations,
                   checkpoint,
                   verbose = FALSE) {

  # Set our current state to the specified intial state.
  s <- s0
  
  # Set the best state we've seen to the intial state.
  best_s <- s0
  best_c <- cost(s)
  
  # Initialize checkpoints
  checks <- list(0, s0, 0)
  names(checks) <- c("Step", "StateHistory", "RunTime")
  StartTime <- Sys.time()
  
  # We always perform a fixed number of iterations.
  for (i in 1:iterations) {
    
    t   <- temperature(Iter = i, MaxIter = iterations)
    s_n <- neighbor(s, TransMatrix = mInvDist, vLower = lower, vUpper = upper)
    c   <- cost(s)
    c_n <- cost(s_n)
    # print("Current state:"); print(s)
    # print("Next state:"); print(s_n)
    # print(paste("Current Cost: ", c, " --- Next Step Cost: ", c_n, sep=""))
    
    if (c_n <= c) { # If the move lowered our cost, save the step
      c <- c_n
      s <- s_n
      if (TRUE == verbose) print(paste("Step ", i, "Accepted"))
    } else { # If the move increased our cost, consider acceptance
      p <- exp(- ((c_n - c) / t)) # The greater the relative c_n cost, the lower the probability of acceptance
      if (TRUE == verbose) paste("Step ", i, ": pr = ", p, sep="")
      if (runif(1) <= p) {
        c <- c_n
        s <- s_n
        if (TRUE == verbose) print(paste("Step ", i, ": Higher Cost Accepted"))
      }
      else if (TRUE == verbose) print(paste("Step ", i, ": Higher Cost Rejected"))
    }
    # If the current step is the best we've seen, save the results
    if (c < best_c) {
      best_s <- s
      best_c <- c
    }

    if (i %% checkpoint == 0 ) {
      t <- difftime(Sys.time(), StartTime, units = "secs")
      print("Step " %&% prettyNum(i, big.mark=",") %&% ": Current vs. Best Cost: (" %&% round(c,1) %&% ", " %&% round(best_c,1) %&%
              "), Temp:" %&% round(t,3) %&% ", Time taken: " %&% round(Time) %&% ", Best State:")
      print(best_s); cat("\n")
      checks$Step         <- cbind(checks$Step, i)
      checks$StateHistory <- cbind(checks$StateHistory, best_s)
      checks$RunTime      <- cbind(checks$RunTime, t)
    }

  }
  
  # Output Results
  SaResults <- list(best_s, checks)
  names(SaResults) <- c("Best State", "CheckPoints")
  return(SaResults)  
  
}
