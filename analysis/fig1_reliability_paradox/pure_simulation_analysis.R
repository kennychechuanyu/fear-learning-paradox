# ================================================================================
# Pure Simulation Reliability Paradox Analysis
# ================================================================================
#
# Demonstrates the reliability paradox using purely simulated participants:
# learning rate parameters show high test-retest reliability while behavioral
# summary measures show poor reliability, even when computed from identical
# underlying parameters.
#
#
# USAGE:
#   1. Run sections 1-7 to define all functions
#   2. Run section 8A to execute bootstrap (SLOW - ~15-30 min)
#   3. Run section 8B to load saved results and create visualizations (FAST)

library(tidyverse)
library(patchwork)
library(future.apply)
library(progressr)

# ICC(2,1) — two-way random, absolute agreement, single measures
icc_2_1 <- function(x, y) {
  ok <- complete.cases(x, y)
  if (sum(ok) < 3) return(NA_real_)
  x <- x[ok]; y <- y[ok]
  if (sd(x) < 1e-10 && sd(y) < 1e-10) return(NA_real_)
  n <- length(x); k <- 2
  dat <- cbind(x, y)
  grand_mean <- mean(dat)
  SS_subj <- k * sum((rowMeans(dat) - grand_mean)^2)
  SS_sess <- n * sum((colMeans(dat) - grand_mean)^2)
  SS_err  <- sum((dat - grand_mean)^2) - SS_subj - SS_sess
  MS_subj <- SS_subj / (n - 1)
  MS_sess <- SS_sess / (k - 1)
  MS_err  <- SS_err / ((n - 1) * (k - 1))
  num <- MS_subj - MS_err
  den <- MS_subj + (k - 1) * MS_err + k * (MS_sess - MS_err) / n
  if (abs(den) < 1e-15) return(NA_real_)
  num / den
}

# ================================================================================
# 1. Parameter Generation
# ================================================================================

generate_participants <- function(n = 200) {
  data.frame(
    alpha = runif(n, 0.05, 0.95),
    w0 = pmin(pmax(rnorm(n, -1, 1.5), -5), 4),
    w1 = runif(n, 2, 8)
  )
}

# ================================================================================
# 2. Conditioning Session Simulation
# ================================================================================

simulate_conditioning_session <- function(design, alpha, w0, w1, A, K, sigma_y) {
  n_trials <- nrow(design)
  v_plus <- 0
  v_minus <- 0
  responses <- numeric(n_trials)

  for (t in seq_len(n_trials)) {
    g <- design$cs_plus[t] * v_plus + design$cs_minus[t] * v_minus
    theta <- A + (K - A) / (1 + exp(-(w0 + w1 * g)))
    responses[t] <- rnorm(1, theta, sigma_y)

    if (design$cs_plus[t] == 1) {
      v_plus <- v_plus + alpha * (design$us_plus[t] - v_plus)
    }
    if (design$cs_minus[t] == 1) {
      v_minus <- v_minus + alpha * (-1 - v_minus)
    }
  }

  responses
}

# ================================================================================
# 3. Experimental Design Generation
# ================================================================================

create_experimental_designs <- function(n_trials = 20,
                                        n_cs_plus = 10,
                                        n_shocks = 8,
                                        manipulation = 'random_sequences') {
  if (manipulation == 'early_vs_late') {
    cs_sequence_A <- sample(rep(c(1, 0), each = n_trials / 2))
    cs_plus_trials_A <- which(cs_sequence_A == 1)
    shock_positions_A <- cs_plus_trials_A[seq_len(n_shocks)]

    cs_sequence_B <- sample(rep(c(1, 0), each = n_trials / 2))
    cs_plus_trials_B <- which(cs_sequence_B == 1)
    shock_positions_B <- cs_plus_trials_B[(length(cs_plus_trials_B) - n_shocks + 1):length(cs_plus_trials_B)]

    design_A <- data.frame(
      cs_plus = cs_sequence_A,
      cs_minus = 1 - cs_sequence_A,
      us_plus = as.numeric(seq_len(n_trials) %in% shock_positions_A)
    )
    design_B <- data.frame(
      cs_plus = cs_sequence_B,
      cs_minus = 1 - cs_sequence_B,
      us_plus = as.numeric(seq_len(n_trials) %in% shock_positions_B)
    )

  } else if (manipulation == 'clustered_vs_distributed') {
    design_A <- data.frame(
      cs_plus = c(rep(1, n_cs_plus), rep(0, n_trials - n_cs_plus)),
      cs_minus = c(rep(0, n_cs_plus), rep(1, n_trials - n_cs_plus)),
      us_plus = c(rep(1, min(n_shocks, n_cs_plus)), rep(0, n_trials - min(n_shocks, n_cs_plus)))
    )

    cs_sequence <- sample(rep(c(1, 0), each = n_trials / 2))
    shock_positions <- sample(which(cs_sequence == 1), n_shocks)
    design_B <- data.frame(
      cs_plus = cs_sequence,
      cs_minus = 1 - cs_sequence,
      us_plus = as.numeric(seq_len(n_trials) %in% shock_positions)
    )

  } else {
    # random_sequences (default)
    for (session in c('A', 'B')) {
      cs_sequence <- sample(rep(c(1, 0), each = n_trials / 2))
      shock_positions <- sample(which(cs_sequence == 1), n_shocks)
      design <- data.frame(
        cs_plus = cs_sequence,
        cs_minus = 1 - cs_sequence,
        us_plus = as.numeric(seq_len(n_trials) %in% shock_positions)
      )
      if (session == 'A') design_A <- design else design_B <- design
    }
  }

  list(A = design_A, B = design_B)
}

# ================================================================================
# 4. Behavioral Metrics
# ================================================================================

calculate_behavioral_metrics <- function(responses, cs_plus_indicator) {
  valid_indices <- complete.cases(responses, cs_plus_indicator)
  responses <- responses[valid_indices]
  n_trials <- length(responses)

  if (n_trials == 0) {
    return(list(slope = NA_real_, delta_first_last = NA_real_, last_trial = NA_real_))
  }

  metrics <- list()

  if (n_trials > 2) {
    trial_numbers <- seq_len(n_trials)
    slope_model <- lm(responses ~ trial_numbers)
    metrics$slope <- coef(slope_model)[2]
  } else {
    metrics$slope <- NA_real_
  }

  metrics$delta_first_last <- responses[n_trials] - responses[1]
  metrics$last_trial <- responses[n_trials]

  metrics
}

# ================================================================================
# 5. Parameter Recovery (MLE)
# ================================================================================

recover_alpha_single_session <- function(responses, design, A, K, w0, w1, sigma_y) {
  log_likelihood <- function(alpha) {
    if (alpha <= 0 || alpha >= 1) return(-Inf)

    total_ll <- 0
    v_plus <- 0
    v_minus <- 0

    for (t in seq_len(nrow(design))) {
      g <- design$cs_plus[t] * v_plus + design$cs_minus[t] * v_minus
      theta <- A + (K - A) / (1 + exp(-(w0 + w1 * g)))
      total_ll <- total_ll + dnorm(responses[t], theta, sigma_y, log = TRUE)

      if (design$cs_plus[t] == 1) {
        v_plus <- v_plus + alpha * (design$us_plus[t] - v_plus)
      }
      if (design$cs_minus[t] == 1) {
        v_minus <- v_minus + alpha * (-1 - v_minus)
      }
    }

    total_ll
  }

  opt <- optimize(function(a) -log_likelihood(a), interval = c(1e-6, 0.999))
  if (!is.finite(opt$objective)) return(NA_real_)
  opt$minimum
}

recover_alpha_joint <- function(responses_A, responses_B, design_A, design_B,
                                A, K, w0, w1, sigma_y) {
  log_likelihood <- function(alpha) {
    if (alpha <= 0 || alpha >= 1) return(-Inf)

    total_ll <- 0

    for (session in 1:2) {
      responses <- if (session == 1) responses_A else responses_B
      design <- if (session == 1) design_A else design_B

      v_plus <- 0
      v_minus <- 0

      for (t in seq_len(nrow(design))) {
        g <- design$cs_plus[t] * v_plus + design$cs_minus[t] * v_minus
        theta <- A + (K - A) / (1 + exp(-(w0 + w1 * g)))
        total_ll <- total_ll + dnorm(responses[t], theta, sigma_y, log = TRUE)

        if (design$cs_plus[t] == 1) {
          v_plus <- v_plus + alpha * (design$us_plus[t] - v_plus)
        }
        if (design$cs_minus[t] == 1) {
          v_minus <- v_minus + alpha * (-1 - v_minus)
        }
      }
    }

    total_ll
  }

  opt <- optimize(function(a) -log_likelihood(a), interval = c(1e-6, 0.999))
  if (!is.finite(opt$objective)) return(NA_real_)
  opt$minimum
}

# ================================================================================
# 5b. Parameter Recovery — Free Response Function (joint alpha, w0, w1)
# ================================================================================

recover_alpha_free_session <- function(responses, design, A, K, sigma_y) {
  neg_log_lik <- function(params) {
    alpha <- params[1]
    w0 <- params[2]
    w1 <- params[3]

    v_plus <- 0
    v_minus <- 0
    total_ll <- 0

    for (t in seq_len(nrow(design))) {
      g <- design$cs_plus[t] * v_plus + design$cs_minus[t] * v_minus
      theta <- A + (K - A) / (1 + exp(-(w0 + w1 * g)))
      total_ll <- total_ll + dnorm(responses[t], theta, sigma_y, log = TRUE)

      if (design$cs_plus[t] == 1) {
        v_plus <- v_plus + alpha * (design$us_plus[t] - v_plus)
      }
      if (design$cs_minus[t] == 1) {
        v_minus <- v_minus + alpha * (-1 - v_minus)
      }
    }
    -total_ll
  }

  starts <- list(c(0.3, -1, 4), c(0.5, 0, 5), c(0.7, -2, 3))
  best <- list(value = Inf)
  for (s in starts) {
    res <- tryCatch(
      optim(s, neg_log_lik, method = "L-BFGS-B",
            lower = c(0.01, -5, 0.5), upper = c(0.99, 4, 15)),
      error = function(e) NULL)
    if (!is.null(res) && res$value < best$value) best <- res
  }

  if (best$value == Inf) return(NA_real_)
  best$par[1]
}

# ================================================================================
# 6. Bootstrap Reliability Analysis
# ================================================================================

run_pure_simulation_bootstrap <- function(n_participants = 200,
                                          n_bootstrap = 200,
                                          n_trials = 20,
                                          n_cs_plus = 10,
                                          n_shocks = 8,
                                          manipulation_types = c('random_sequences',
                                                                'early_vs_late',
                                                                'clustered_vs_distributed'),
                                          A_param = 0,
                                          K_param = 100,
                                          sigma_y = 5,
                                          parallel = TRUE,
                                          show_progress = TRUE) {
  workers <- max(1, parallel::detectCores() - 1)
  previous_plan <- future::plan()
  if (parallel) {
    future::plan(future::multisession, workers = workers)
  } else {
    future::plan(future::sequential)
  }
  on.exit(future::plan(previous_plan), add = TRUE)

  if (show_progress) {
    progressr::handlers(global = TRUE)
  }

  behavioral_metrics <- c('slope', 'delta_first_last', 'last_trial')
  all_results <- list()

  for (manipulation in manipulation_types) {

    iteration_fn <- function(b, progress = NULL) {
      if (!is.null(progress)) {
        progress(message = sprintf('%s [%d/%d]', manipulation, b, n_bootstrap))
      }

      participants <- generate_participants(n_participants)

      true_alphas <- numeric(n_participants)
      recovered_alpha_A <- numeric(n_participants)
      recovered_alpha_B <- numeric(n_participants)
      recovered_alpha_joint <- numeric(n_participants)
      recovered_alpha_free_A <- numeric(n_participants)
      recovered_alpha_free_B <- numeric(n_participants)

      metrics_storage <- list()
      for (metric in behavioral_metrics) {
        metrics_storage[[paste0(metric, '_A')]] <- numeric(n_participants)
        metrics_storage[[paste0(metric, '_B')]] <- numeric(n_participants)
      }

      for (i in seq_len(n_participants)) {
        alpha <- participants$alpha[i]
        w0 <- participants$w0[i]
        w1 <- participants$w1[i]
        true_alphas[i] <- alpha

        designs <- create_experimental_designs(
          n_trials = n_trials,
          n_cs_plus = n_cs_plus,
          n_shocks = n_shocks,
          manipulation = manipulation
        )

        resp_A <- simulate_conditioning_session(
          designs[['A']], alpha, w0, w1, A_param, K_param, sigma_y
        )
        resp_B <- simulate_conditioning_session(
          designs[['B']], alpha, w0, w1, A_param, K_param, sigma_y
        )

        recovered_alpha_A[i] <- recover_alpha_single_session(
          resp_A, designs[['A']], A_param, K_param, w0, w1, sigma_y
        )
        recovered_alpha_B[i] <- recover_alpha_single_session(
          resp_B, designs[['B']], A_param, K_param, w0, w1, sigma_y
        )

        recovered_alpha_joint[i] <- recover_alpha_joint(
          resp_A, resp_B, designs[['A']], designs[['B']],
          A_param, K_param, w0, w1, sigma_y
        )

        recovered_alpha_free_A[i] <- recover_alpha_free_session(
          resp_A, designs[['A']], A_param, K_param, sigma_y
        )
        recovered_alpha_free_B[i] <- recover_alpha_free_session(
          resp_B, designs[['B']], A_param, K_param, sigma_y
        )

        bm_A <- calculate_behavioral_metrics(resp_A, designs[['A']]$cs_plus)
        bm_B <- calculate_behavioral_metrics(resp_B, designs[['B']]$cs_plus)

        for (metric in c('slope', 'delta_first_last', 'last_trial')) {
          metrics_storage[[paste0(metric, '_A')]][i] <- bm_A[[metric]]
          metrics_storage[[paste0(metric, '_B')]][i] <- bm_B[[metric]]
        }


      }

      correlations <- list()
      correlations$alpha_test_retest <- icc_2_1(recovered_alpha_A, recovered_alpha_B)
      correlations$alpha_free_trt <- icc_2_1(recovered_alpha_free_A, recovered_alpha_free_B)
      correlations$alpha_recovery <- cor(true_alphas, recovered_alpha_joint,
                                          use = 'complete.obs')

      for (metric in behavioral_metrics) {
        vals_A <- metrics_storage[[paste0(metric, '_A')]]
        vals_B <- metrics_storage[[paste0(metric, '_B')]]
        valid <- complete.cases(vals_A, vals_B)
        correlations[[metric]] <- if (sum(valid) >= 3) {
          icc_2_1(vals_A[valid], vals_B[valid])
        } else {
          NA_real_
        }
      }

      correlations
    }

    bootstrap_results <- if (show_progress) {
      progressr::with_progress({
        progress_cb <- progressr::progressor(steps = n_bootstrap, label = manipulation)
        future.apply::future_lapply(
          seq_len(n_bootstrap),
          function(b) iteration_fn(b, progress_cb),
          future.seed = TRUE
        )
      })
    } else {
      future.apply::future_lapply(seq_len(n_bootstrap), iteration_fn, future.seed = TRUE)
    }

    all_metrics <- c('alpha_test_retest', 'alpha_free_trt', 'alpha_recovery', behavioral_metrics)
    corr_matrix <- matrix(NA_real_, n_bootstrap, length(all_metrics))
    colnames(corr_matrix) <- all_metrics

    for (b in seq_len(n_bootstrap)) {
      for (metric in all_metrics) {
        if (metric %in% names(bootstrap_results[[b]])) {
          corr_matrix[b, metric] <- bootstrap_results[[b]][[metric]]
        }
      }
    }

    condition_name <- paste0(n_trials, 'trials_', manipulation)
    all_results[[condition_name]] <- list(
      correlations = corr_matrix,
      manipulation = manipulation,
      n_trials = n_trials,
      n_bootstrap = n_bootstrap,
      sample_size = n_participants
    )
  }

  all_results
}

# ================================================================================
# 7. Summary
# ================================================================================

summarize_results <- function(bootstrap_results) {
  summary_data <- data.frame()

  for (condition_name in names(bootstrap_results)) {
    result <- bootstrap_results[[condition_name]]
    correlations <- result$correlations

    for (metric in colnames(correlations)) {
      values <- correlations[, metric]
      values <- values[!is.na(values)]

      if (length(values) > 0) {
        summary_data <- rbind(summary_data, data.frame(
          condition = condition_name,
          n_trials = result$n_trials,
          manipulation = result$manipulation,
          metric = metric,
          mean_r = mean(values),
          median_r = median(values),
          sd_r = sd(values),
          ci_lower = quantile(values, 0.025),
          ci_upper = quantile(values, 0.975),
          n_bootstrap = length(values),
          stringsAsFactors = FALSE
        ))
      }
    }
  }

  summary_data
}

# ================================================================================
# 8A. BOOTSTRAP EXECUTION (RUN ONCE - SLOW)
# ================================================================================

results_short <- run_pure_simulation_bootstrap(
  n_participants = 200,
  n_bootstrap = 200,
  n_trials = 20,
  n_cs_plus = 10,
  n_shocks = 8,
  A_param = 0,
  K_param = 100,
  sigma_y = 5,
  parallel = TRUE,
  show_progress = TRUE
)

results_standard <- run_pure_simulation_bootstrap(
  n_participants = 200,
  n_bootstrap = 200,
  n_trials = 40,
  n_cs_plus = 20,
  n_shocks = 15,
  A_param = 0,
  K_param = 100,
  sigma_y = 5,
  parallel = TRUE,
  show_progress = TRUE
)

all_bootstrap_results <- c(results_short, results_standard)

save(all_bootstrap_results,
     file = 'pure_simulation_results.RData')


# ================================================================================
# 8B. EXPORT SUMMARY (RUN AFTER LOADING SAVED RESULTS)
# ================================================================================


# Uncomment to load saved results:
# load('pure_simulation_results.RData')

summary_data <- summarize_results(all_bootstrap_results)
write.csv(summary_data, 'pure_simulation_summary.csv', row.names = FALSE)

