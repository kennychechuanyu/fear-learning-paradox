# ================================================================================
# Core Rescorla-Wagner Model Functions
# ================================================================================
#
# Shared simulation functions extracted from proof-of-concept analyses
# Used across all Shiny app modules
#
# Author: Kenny Yu
# Date: October 2025
# ================================================================================

#' Simulate Single Conditioning Session
#'
#' Generates trial-by-trial responses using Rescorla-Wagner learning model
#' with sigmoidal response transformation
#'
#' @param alpha Learning rate (0-1)
#' @param n_trials Number of trials
#' @param shock_trials Vector of trial numbers where shock occurs
#' @param w0 Response function intercept (default: -1)
#' @param w1 Response function slope (default: 4)
#' @param A Response minimum bound (default: 0)
#' @param K Response maximum bound (default: 100)
#' @param sigma Observation noise SD (default: 5)
#'
#' @return Data frame with columns: trial, response, value, shock
simulate_rw_session <- function(alpha,
                                n_trials = 30,
                                shock_trials,
                                w0 = -1,
                                w1 = 4,
                                A = 0,
                                K = 100,
                                sigma = 5) {

  # Initialize associative value
  v <- 0

  # Storage for trial-by-trial data
  responses <- numeric(n_trials)
  values <- numeric(n_trials)

  for (t in 1:n_trials) {
    # Transform associative value to response scale (sigmoid)
    theta <- A + (K - A) / (1 + exp(-(w0 + w1 * v)))

    # Generate observed response with noise
    responses[t] <- rnorm(1, theta, sigma)

    # Store current value
    values[t] <- v

    # Update value based on prediction error (Rescorla-Wagner rule)
    if (t %in% shock_trials) {
      # Positive prediction error (US present)
      v <- v + alpha * (1 - v)
    } else {
      # Negative prediction error (US absent / extinction)
      v <- v + alpha * (0 - v)
    }
  }

  # Return structured data
  data.frame(
    trial = 1:n_trials,
    response = pmax(A, pmin(K, responses)),  # Enforce bounds
    value = values,
    shock = 1:n_trials %in% shock_trials
  )
}


#' Simulate CS+/CS- Discrimination Design
#'
#' Generates responses for typical conditioning design with CS+ and CS- trials
#' Used for reliability paradox and identifiability analyses
#'
#' @param design Data frame with cs_plus, cs_minus, us_plus columns
#' @param alpha Learning rate
#' @param w0 Response intercept
#' @param w1 Response slope
#' @param A Response minimum
#' @param K Response maximum
#' @param sigma_y Observation noise
#'
#' @return Vector of responses (length = nrow(design))
simulate_discrimination_session <- function(design,
                                           alpha,
                                           w0,
                                           w1,
                                           A = 0,
                                           K = 100,
                                           sigma_y = 2) {

  n_trials <- nrow(design)
  v_plus <- 0    # CS+ value
  v_minus <- 0   # CS- value
  responses <- numeric(n_trials)

  for (t in seq_len(n_trials)) {
    # Calculate expected value for current trial (CS+ or CS-)
    g <- design$cs_plus[t] * v_plus + design$cs_minus[t] * v_minus

    # Transform to response scale
    theta <- A + (K - A) / (1 + exp(-(w0 + w1 * g)))

    # Observed response with noise
    responses[t] <- rnorm(1, theta, sigma_y)

    # Update values using Rescorla-Wagner rule
    if (design$cs_plus[t] == 1) {
      # CS+ trial: learn about shock presence/absence
      v_plus <- v_plus + alpha * (design$us_plus[t] - v_plus)
    }
    if (design$cs_minus[t] == 1) {
      # CS- trial: learn shock is absent (value = -1)
      v_minus <- v_minus + alpha * (-1 - v_minus)
    }
  }

  # Enforce response bounds
  pmax(A, pmin(K, responses))
}


#' Calculate Behavioral Summary Indices
#'
#' Extracts common behavioral metrics used in conditioning research
#'
#' @param responses Vector of trial-by-trial responses
#'
#' @return List with mean, slope, first_last_diff, final_trial
calculate_behavioral_indices <- function(responses) {

  n_trials <- length(responses)

  # Remove missing data
  valid_responses <- responses[!is.na(responses)]

  if (length(valid_responses) < 3) {
    return(list(
      mean = NA_real_,
      slope = NA_real_,
      first_last_diff = NA_real_,
      final_trial = NA_real_
    ))
  }

  # Mean response level
  mean_resp <- mean(valid_responses, na.rm = TRUE)

  # Linear trend (slope)
  trial_seq <- seq_along(valid_responses)
  slope_model <- lm(valid_responses ~ trial_seq)
  slope <- coef(slope_model)[2]

  # Change from first to last trial
  first_last_diff <- valid_responses[length(valid_responses)] - valid_responses[1]

  # Final trial response
  final_trial <- valid_responses[length(valid_responses)]

  list(
    mean = mean_resp,
    slope = slope,
    first_last_diff = first_last_diff,
    final_trial = final_trial
  )
}


#' Create Experimental Design
#'
#' Generates CS+/CS- trial sequences for conditioning experiments
#'
#' @param n_trials Total number of trials
#' @param reinforcement_rate Proportion of CS+ trials that are shocked (0-1)
#' @param ordering "random", "blocked", or "alternating"
#' @param ratio CS+:CS- ratio ("1:1", "1:2", "2:1")
#'
#' @return Data frame with cs_plus, cs_minus, us_plus columns
create_experimental_design <- function(n_trials = 20,
                                      reinforcement_rate = 0.8,
                                      ordering = "random",
                                      ratio = "1:1") {

  # Determine CS+ and CS- trial counts based on ratio
  if (ratio == "1:1") {
    n_cs_plus <- round(n_trials / 2)
  } else if (ratio == "1:2") {
    n_cs_plus <- round(n_trials / 3)
  } else if (ratio == "2:1") {
    n_cs_plus <- round(2 * n_trials / 3)
  } else {
    stop("Ratio must be '1:1', '1:2', or '2:1'")
  }

  n_cs_minus <- n_trials - n_cs_plus

  # Create trial sequence based on ordering
  if (ordering == "blocked") {
    cs_sequence <- c(rep(1, n_cs_plus), rep(0, n_cs_minus))
  } else if (ordering == "alternating") {
    cs_sequence <- rep(c(1, 0), length.out = n_trials)
    # Adjust to match exact counts
    while (sum(cs_sequence) > n_cs_plus) {
      cs_sequence[which(cs_sequence == 1)[length(which(cs_sequence == 1))]] <- 0
    }
    while (sum(cs_sequence) < n_cs_plus) {
      cs_sequence[which(cs_sequence == 0)[1]] <- 1
    }
  } else {  # random (default)
    cs_sequence <- sample(c(rep(1, n_cs_plus), rep(0, n_cs_minus)))
  }

  # Determine which CS+ trials are reinforced (shocked)
  cs_plus_trials <- which(cs_sequence == 1)
  n_shocks <- round(reinforcement_rate * n_cs_plus)
  shocked_trials <- sample(cs_plus_trials, n_shocks)

  # Build design data frame
  design <- data.frame(
    cs_plus = cs_sequence,
    cs_minus = 1 - cs_sequence,
    us_plus = as.numeric(seq_len(n_trials) %in% shocked_trials)
  )

  design
}


#' Recover Learning Rate from Observed Responses
#'
#' Estimates alpha using maximum likelihood estimation from single session
#' Assumes w0, w1, sigma are known (fixed parameters)
#'
#' @param responses Vector of observed responses
#' @param shock_trials Vector of trial indices where shock occurred
#' @param n_trials Total number of trials
#' @param w0 Response intercept (fixed)
#' @param w1 Response slope (fixed)
#' @param A Response minimum bound
#' @param K Response maximum bound
#' @param sigma Observation noise (fixed)
#'
#' @return Estimated alpha (learning rate)
recover_alpha_mle <- function(responses,
                              shock_trials,
                              n_trials,
                              w0 = -1,
                              w1 = 4,
                              A = 0,
                              K = 100,
                              sigma = 5) {

  if (any(is.na(responses))) {
    return(NA_real_)
  }

  log_likelihood <- function(alpha) {
    if (alpha <= 0 || alpha >= 1) return(-Inf)

    v <- 0
    ll <- 0

    for (t in 1:n_trials) {
      theta <- A + (K - A) / (1 + exp(-(w0 + w1 * v)))

      ll <- ll + dnorm(responses[t], mean = theta, sd = sigma, log = TRUE)

      if (t %in% shock_trials) {
        v <- v + alpha * (1 - v)
      } else {
        v <- v + alpha * (0 - v)
      }
    }

    ll
  }

  result <- tryCatch({
    opt <- optimize(
      f = function(a) -log_likelihood(a),
      interval = c(0.01, 0.99),
      maximum = FALSE
    )
    opt$minimum
  }, error = function(e) {
    NA_real_
  })

  result
}


#' Simulate CS+/CS- Discrimination with Generalization
#'
#' Generates trial-by-trial responses with both direct learning and generalization
#' between CS+ and CS- stimuli
#'
#' @param alpha Learning rate (0-1)
#' @param generalization Generalization parameter (0-1)
#' @param n_trials Number of trials
#' @param cs_plus_trials Vector of trial numbers for CS+ presentation
#' @param us_trials Vector of trial numbers where US (shock) occurs
#' @param w0 Response function intercept (default: -1)
#' @param w1 Response function slope (default: 4)
#' @param A Response minimum bound (default: 0)
#' @param K Response maximum bound (default: 100)
#' @param sigma Observation noise SD (default: 3)
#'
#' @return Data frame with columns: trial, stimulus, response, value_plus, value_minus, shock
simulate_discrimination_generalization <- function(alpha,
                                                   generalization,
                                                   n_trials = 40,
                                                   cs_plus_trials,
                                                   us_trials,
                                                   w0 = -1,
                                                   w1 = 4,
                                                   A = 0,
                                                   K = 100,
                                                   sigma = 3) {

  # Initialize associative values
  v_plus <- 0   # CS+ value
  v_minus <- 0  # CS- value

  # Storage
  responses <- numeric(n_trials)
  values_plus <- numeric(n_trials)
  values_minus <- numeric(n_trials)
  stimulus <- character(n_trials)

  for (t in 1:n_trials) {
    # Determine current stimulus
    is_cs_plus <- t %in% cs_plus_trials
    is_us <- t %in% us_trials

    # Get current value for response generation
    current_v <- if (is_cs_plus) v_plus else v_minus

    # Transform to response scale
    theta <- A + (K - A) / (1 + exp(-(w0 + w1 * current_v)))

    # Generate observed response
    responses[t] <- rnorm(1, theta, sigma)

    # Store values
    values_plus[t] <- v_plus
    values_minus[t] <- v_minus
    stimulus[t] <- if (is_cs_plus) "CS+" else "CS-"

    # Update values with learning and generalization
    if (is_cs_plus) {
      # CS+ trial: direct learning for CS+, generalized learning for CS-
      us_present <- if (is_us) 1 else 0
      pe <- us_present - v_plus  # prediction error

      v_plus <- v_plus + alpha * pe  # direct learning
      v_minus <- v_minus + generalization * alpha * pe  # generalized learning

    } else {
      # CS- trial: direct learning for CS-, generalized learning for CS+
      # CS- is always safe (US = -1 to create inhibitory learning)
      pe <- -1 - v_minus  # prediction error (safety signal)

      v_minus <- v_minus + alpha * pe  # direct learning
      v_plus <- v_plus + generalization * alpha * pe  # generalized learning
    }
  }

  data.frame(
    trial = 1:n_trials,
    stimulus = stimulus,
    response = pmax(A, pmin(K, responses)),
    value_plus = values_plus,
    value_minus = values_minus,
    shock = 1:n_trials %in% us_trials
  )
}


#' Simulate Session with Hierarchical Alpha (Acquisition vs Extinction)
#'
#' Generates responses where learning rate differs between acquisition and extinction phases
#'
#' @param alpha_acq Learning rate during acquisition (when shocks occur)
#' @param alpha_ext Learning rate during extinction (no shocks)
#' @param n_trials_acq Number of acquisition trials
#' @param n_trials_ext Number of extinction trials
#' @param n_shocks Number of shocks during acquisition
#' @param w0 Response function intercept
#' @param w1 Response function slope
#' @param A Response minimum bound
#' @param K Response maximum bound
#' @param sigma Observation noise SD
#'
#' @return Data frame with trial, phase, response, value, shock columns
simulate_hierarchical_alpha <- function(alpha_acq,
                                       alpha_ext,
                                       n_trials_acq = 20,
                                       n_trials_ext = 10,
                                       n_shocks = 14,
                                       w0 = -1,
                                       w1 = 4,
                                       A = 0,
                                       K = 100,
                                       sigma = 3) {

  n_trials <- n_trials_acq + n_trials_ext

  # Shocks occur during acquisition phase
  shock_trials <- sort(sample(1:n_trials_acq, min(n_shocks, n_trials_acq)))

  # Initialize
  v <- 0
  responses <- numeric(n_trials)
  values <- numeric(n_trials)
  phases <- character(n_trials)

  for (t in 1:n_trials) {
    # Determine phase
    is_acquisition <- t <= n_trials_acq
    phases[t] <- if (is_acquisition) "Acquisition" else "Extinction"

    # Transform to response
    theta <- A + (K - A) / (1 + exp(-(w0 + w1 * v)))
    responses[t] <- rnorm(1, theta, sigma)
    values[t] <- v

    # Update with phase-specific learning rate
    current_alpha <- if (is_acquisition) alpha_acq else alpha_ext

    if (t %in% shock_trials) {
      v <- v + current_alpha * (1 - v)  # shock present
    } else {
      v <- v + current_alpha * (0 - v)  # shock absent
    }
  }

  data.frame(
    trial = 1:n_trials,
    phase = phases,
    response = pmax(A, pmin(K, responses)),
    value = values,
    shock = 1:n_trials %in% shock_trials
  )
}


#' Fit Fixed Alpha Model to Hierarchical Data
#'
#' Fits a simple fixed-alpha RW model to data generated with hierarchical alpha
#' Used to demonstrate model misspecification
#'
#' @param responses Vector of observed responses
#' @param shock_trials Vector of trials where shock occurred
#' @param n_trials Total number of trials
#'
#' @return Estimated alpha value
fit_fixed_alpha <- function(responses, shock_trials, n_trials) {

  # Fixed parameters (assumed known)
  w0 <- -1
  w1 <- 4
  A <- 0
  K <- 100
  sigma <- 3

  # Log-likelihood function
  log_lik <- function(alpha) {
    if (alpha <= 0.01 || alpha >= 0.99) return(-Inf)

    v <- 0
    ll <- 0

    for (t in 1:n_trials) {
      theta <- A + (K - A) / (1 + exp(-(w0 + w1 * v)))
      ll <- ll + dnorm(responses[t], mean = theta, sd = sigma, log = TRUE)

      if (t %in% shock_trials) {
        v <- v + alpha * (1 - v)
      } else {
        v <- v + alpha * (0 - v)
      }
    }

    ll
  }

  # Optimize
  result <- tryCatch({
    opt <- optimize(
      f = function(a) -log_lik(a),
      interval = c(0.01, 0.99),
      maximum = FALSE
    )
    opt$minimum
  }, error = function(e) {
    NA_real_
  })

  result
}


#' Simulate Two Sessions with Memory Carry-Over
#'
#' Generates responses for two learning sessions where Session 2 starts
#' with elevated prior belief based on Session 1 learning
#'
#' @param alpha Learning rate
#' @param memory_retention Proportion of Session 1 learning carried to Session 2 (0-1)
#' @param n_trials_s1 Number of trials in Session 1
#' @param n_trials_s2 Number of trials in Session 2
#' @param shock_rate Proportion of trials with shock
#' @param w0 Response intercept
#' @param w1 Response slope
#' @param A Response minimum
#' @param K Response maximum
#' @param sigma Observation noise
#'
#' @return Data frame with session, trial, response, value columns
simulate_two_sessions_carryover <- function(alpha,
                                            memory_retention = 0.8,
                                            n_trials_s1 = 15,
                                            n_trials_s2 = 15,
                                            shock_rate = 0.7,
                                            w0 = -1,
                                            w1 = 4,
                                            A = 0,
                                            K = 100) {

  # Sigmoidal response function (consistent with main analysis)
  response_fn <- function(v) {
    A + (K - A) / (1 + exp(-(w0 + w1 * v)))
  }

  # SESSION 1: Start from zero (naive)
  v_s1 <- 0
  session1_data <- data.frame(
    session = integer(),
    trial = integer(),
    response = numeric(),
    value = numeric(),
    shock = logical()
  )

  shock_trials_s1 <- sort(sample(1:n_trials_s1, round(n_trials_s1 * shock_rate)))

  for (t in 1:n_trials_s1) {
    # Generate response using sigmoidal transformation
    response_t <- response_fn(v_s1)

    # Store trial data
    session1_data <- rbind(session1_data, data.frame(
      session = 1,
      trial = t,
      response = response_t,
      value = v_s1,
      shock = t %in% shock_trials_s1
    ))

    # Update v for NEXT trial based on outcome
    if (t %in% shock_trials_s1) {
      v_s1 <- v_s1 + alpha * (1 - v_s1)
    } else {
      v_s1 <- v_s1 + alpha * (0 - v_s1)
    }
  }

  # SESSION 2: Start with memory carry-over
  # Use the LAST response's v value from Session 1, scaled by memory retention
  v_s1_final <- session1_data$value[nrow(session1_data)]
  v_s2 <- v_s1_final * memory_retention

  session2_data <- data.frame(
    session = integer(),
    trial = integer(),
    response = numeric(),
    value = numeric(),
    shock = logical()
  )

  shock_trials_s2 <- sort(sample(1:n_trials_s2, round(n_trials_s2 * shock_rate)))

  for (t in 1:n_trials_s2) {
    # Generate response using sigmoidal transformation
    response_t <- response_fn(v_s2)

    # Store trial data
    session2_data <- rbind(session2_data, data.frame(
      session = 2,
      trial = t,
      response = response_t,
      value = v_s2,
      shock = t %in% shock_trials_s2
    ))

    # Update v for NEXT trial
    if (t %in% shock_trials_s2) {
      v_s2 <- v_s2 + alpha * (1 - v_s2)
    } else {
      v_s2 <- v_s2 + alpha * (0 - v_s2)
    }
  }

  # Combine both sessions
  bind_rows(session1_data, session2_data)
}


# ================================================================================
# Pearce-Hall Model Functions
# ================================================================================
#
# The Pearce-Hall model differs from RW in that learning rate (attention) is
# dynamically updated based on prediction errors. High surprise → high attention.
# After outcomes become predictable, attention decreases and learning slows.
#
# ================================================================================

#' Simulate Pearce-Hall Learning Session
#'
#' Generates trial-by-trial responses using Pearce-Hall model where
#' learning rate (attention) is modulated by recent prediction errors
#'
#' @param alpha_initial Initial learning rate / base learning rate
#' @param kappa Attention decay rate (0-1); higher = faster decay of attention
#' @param n_trials Number of trials
#' @param shock_trials Vector of trial numbers where shock occurs
#' @param w0 Response function intercept (default: -1)
#' @param w1 Response function slope (default: 4)
#' @param A Response minimum bound (default: 0)
#' @param K Response maximum bound (default: 100)
#' @param sigma Observation noise SD (default: 3)
#'
#' @return Data frame with columns: trial, response, value, attention, shock
simulate_ph_session <- function(alpha_initial,
                                kappa = 0.5,
                                n_trials = 30,
                                shock_trials,
                                w0 = -1,
                                w1 = 4,
                                A = 0,
                                K = 100,
                                sigma = 3) {

  # Initialize associative value and attention
  v <- 0
  attention <- alpha_initial  # Start with high attention

  # Storage
  responses <- numeric(n_trials)
  values <- numeric(n_trials)
  attentions <- numeric(n_trials)

  for (t in 1:n_trials) {
    # Transform associative value to response scale
    theta <- A + (K - A) / (1 + exp(-(w0 + w1 * v)))

    # Generate observed response with noise
    responses[t] <- rnorm(1, theta, sigma)

    # Store current state
    values[t] <- v
    attentions[t] <- attention

    # Determine outcome for this trial
    outcome <- if (t %in% shock_trials) 1 else 0

    # Calculate prediction error (absolute)
    pe <- outcome - v

    # Update value with CURRENT attention level (Pearce-Hall rule)
    v <- v + attention * pe

    # Update attention for NEXT trial based on absolute prediction error
    # High surprise → attention stays high
    # Low surprise → attention decays
    attention <- kappa * abs(pe) + (1 - kappa) * attention
  }

  data.frame(
    trial = 1:n_trials,
    response = pmax(A, pmin(K, responses)),
    value = values,
    attention = attentions,
    shock = 1:n_trials %in% shock_trials
  )
}


#' Fit Misspecified RW Model to PH Data
#'
#' Fits a fixed-alpha RW model to data generated by Pearce-Hall model
#' Demonstrates model misspecification error
#'
#' @param responses Vector of observed responses
#' @param shock_trials Vector of trials where shock occurred
#' @param n_trials Total number of trials
#' @param w0 Response intercept (fixed)
#' @param w1 Response slope (fixed)
#' @param A Response minimum
#' @param K Response maximum
#' @param sigma Observation noise (fixed)
#'
#' @return Estimated alpha value from misspecified RW model
fit_rw_to_ph_data <- function(responses,
                               shock_trials,
                               n_trials,
                               w0 = -1,
                               w1 = 4,
                               A = 0,
                               K = 100,
                               sigma = 3) {

  # Log-likelihood for RW model (wrong model!)
  log_lik <- function(alpha) {
    if (alpha <= 0.01 || alpha >= 0.99) return(-Inf)

    v <- 0
    ll <- 0

    for (t in 1:n_trials) {
      theta <- A + (K - A) / (1 + exp(-(w0 + w1 * v)))
      ll <- ll + dnorm(responses[t], mean = theta, sd = sigma, log = TRUE)

      outcome <- if (t %in% shock_trials) 1 else 0
      v <- v + alpha * (outcome - v)
    }

    ll
  }

  # Optimize
  result <- tryCatch({
    opt <- optimize(
      f = function(a) -log_lik(a),
      interval = c(0.01, 0.99),
      maximum = FALSE
    )
    opt$minimum
  }, error = function(e) {
    NA_real_
  })

  result
}


#' Generate RW Model Predictions
#'
#' Simulates learning trajectory using RW model with given alpha
#' Used for plotting fitted model predictions
#'
#' @param alpha Learning rate
#' @param shock_trials Vector of trials where shock occurs
#' @param n_trials Total number of trials
#'
#' @return Vector of predicted values (associative strength)
predict_rw_trajectory <- function(alpha, shock_trials, n_trials) {

  v <- 0
  values <- numeric(n_trials)

  for (t in 1:n_trials) {
    values[t] <- v

    outcome <- if (t %in% shock_trials) 1 else 0
    v <- v + alpha * (outcome - v)
  }

  values
}

