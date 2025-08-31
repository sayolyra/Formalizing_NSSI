

########################### INSTALLING TOOLS:
library(dplyr)
library(ggplot2)
library(tidyr)

########################### FUNCTION INPUT:

# person inputs:
#   integer vector indicating the time_vec interval,
#   stepsize (<=1) for smooth calibration,
#   parameter values for the model,
#   initial values for state variables,
#   seed for reproducible results.

simNSSI <- function(time_vec = NULL,
                    # time_vec interval
                    stepsize = NULL,
                    # how often changes occur per 1 time_vec unit (minute)
                    PS = NULL,
                    # variables with fixed values
                    INI = NULL,
                    # variables with values that change over time_vec
                    seed = NULL)

{
  ########################### SET-UP ###########################

  #### Step 1: Set seed and timescale ---------------
  if (!is.null(seed)) {
    set.seed(seed)
  }
  # Range of time_vec steps, i.e. number of specified iterations
  range_time <- range(time_vec)


  #### Step 2: Specify state variables' initial starting values ---------------

  A  <- INI$A # HANE
  U <- INI$U # Urge
  P <- INI$P # Pain
  PN <- INI$PN # Probability of engaging in N
  N <- INI$N # NSSI


  #### Step 3: Specify auxillary variables  ---------------

  timepoints <- seq(time_vec[1], max(time_vec), by = stepsize)

  # stress vector
  E <- rep(0, length(timepoints))
  E[round(300 / stepsize):round(345 / stepsize)] = 1 # when in time_vec frame stressor occurs (45 second stressor)

  # Create buffer period 60 seconds after stressor ends
  stressor_end <- 345  # end of stressor in seconds
  buffer_end <- stressor_end + 60
  buffer_endpoint <- round(buffer_end / stepsize)

  last_N <- -Inf

  # Save user-inputted values
  outmat <- matrix(
    NA,
    nrow = length(timepoints),
    ncol = 7,
    dimnames = list(NULL, c("timepoints", "E", "A", "U", "P", "PN", "N"))
  )
  outmat[1, ] <- c(timepoints[1], E[1], A, U, P, PN, N)

  # Track Time with a time_vec-tracker
  obs_tracker <-
    2 # since the first observation has already been taken, the next save point is the 2nd


  ######################### SIMULATION ###########################
  # 'For Loop' that carries out simulation

  for (time_tracker in 2:length(timepoints)) {

  # HANE
    Anew <- A + dA_dt(A = A,
                      E = E[time_tracker - 1],
                      k_SS = PS[["A"]][["k_SSA"]],
                      r_A = PS[["A"]][["r_A"]],
                      r_AP = PS[["A"]][["r_AP"]]) * stepsize

    Anew <- ifelse(Anew < 0, 0, Anew)

  # NSSI
    # Only allow NSSI if:
    #     (a) distressing event buffer period has passed, AND
    #     (b) at least 1 second has passed since last N event
    if (time_tracker <= buffer_endpoint ||
        (timepoints[time_tracker] - last_N) < 5) {
      Nnew <- 0
    } else {
      Nnew <- rbinom(1, 1, prob = PN * stepsize)
    }

    # If NSSI occurs, update last_N:
    if (Nnew == 1) {
      last_N <- timepoints[time_tracker]
    }

  # Urge
    Unew <- U + dU_dt(U = U,
                      A = A,
                      k_SS = PS[["U"]][["k_SSU"]],
                      r_U = PS[["U"]][["r_U"]],
                      m_U = PS[["U"]][["m_U"]]) * stepsize

  # Pain
    Pnew <- P + dP_dt(P = P,
                      N = N,
                      r_P = PS[["P"]][["r_P"]],
                      k_PS = PS[["P"]][["k_PS"]]) * stepsize

  # Probability of N
    PNnew <- PN + dPN_dt(PN = PN,
                         U = U,
                         k_I = PS[["PN"]][["k_I"]],
                         m_N = PS[["PN"]][["m_N"]],
                         r_PN = PS[["PN"]][["r_PN"]]) * stepsize

    PNnew <- ifelse(PNnew >1, 1, PNnew)

    # Overwrite current values
    A <- Anew
    N <- Nnew
    U <- Unew
    P <- Pnew
    PN <- PNnew

    # Save results
    outmat[time_tracker, ] <-
      c(timepoints[time_tracker], E[time_tracker], A, U, P, PN, N)
  } # End: for loop over time_vec steps

  ########################### END OF SIMULATION ###########################