# ================================================================================
# Alpha Identifiability Simulation
# ================================================================================
#
# Purpose: Systematically evaluate how experimental design factors influence
#          the identifiability of learning rate (alpha) parameter
#
# Uses UNIFORM alpha distribution [0.1, 0.9] to test identifiability across
# the full theoretical range.
#
# Author: Kenny Yu
# Date: October 2025
# ================================================================================

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,
  future,
  future.apply,
  progressr,
  patchwork,
  viridis
)

set.seed(42)

# ================================================================================
# 1. Core Simulation Functions (adapted from reliability_paradox_bootstrap_analysis.R)
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

    # Rescorla-Wagner updates
    if (design$cs_plus[t] == 1) {
      v_plus <- v_plus + alpha * (design$us_plus[t] - v_plus)
    }
    if (design$cs_minus[t] == 1) {
      v_minus <- v_minus + alpha * (-1 - v_minus)
    }
  }

  responses
}


create_experimental_design <- function(n_trials,
                                      reinforcement_rate,
                                      ordering,
                                      ratio) {

  if (ratio == "1:1") {
    n_cs_plus <- n_trials / 2
  } else if (ratio == "1:2") {
    n_cs_plus <- n_trials / 3
  } else if (ratio == "2:1") {
    n_cs_plus <- (2 * n_trials) / 3
  }

  n_cs_plus <- round(n_cs_plus)
  n_cs_minus <- n_trials - n_cs_plus

  # Create trial sequence based on ordering
  if (ordering == "blocked") {
    # All CS+ trials first, then CS- trials
    cs_sequence <- c(rep(1, n_cs_plus), rep(0, n_cs_minus))
  } else if (ordering == "alternating") {
    # Alternate as much as possible
    cs_sequence <- rep(c(1, 0), length.out = n_trials)
    # Adjust to match exact counts
    n_ones <- sum(cs_sequence)
    if (n_ones > n_cs_plus) {
      # Remove extra 1s
      ones_idx <- which(cs_sequence == 1)
      cs_sequence[sample(ones_idx, n_ones - n_cs_plus)] <- 0
    } else if (n_ones < n_cs_plus) {
      # Add more 1s
      zeros_idx <- which(cs_sequence == 0)
      cs_sequence[sample(zeros_idx, n_cs_plus - n_ones)] <- 1
    }
  } else {
    # Random ordering
    cs_sequence <- sample(rep(c(1, 0), c(n_cs_plus, n_cs_minus)))
  }

  n_shocks <- round(n_cs_plus * reinforcement_rate)
  cs_plus_trials <- which(cs_sequence == 1)
  shock_trials <- sample(cs_plus_trials, min(n_shocks, length(cs_plus_trials)))

  us_sequence <- numeric(n_trials)
  us_sequence[shock_trials] <- 1

  data.frame(
    cs_plus = cs_sequence,
    cs_minus = 1 - cs_sequence,
    us_plus = us_sequence
  )
}


recover_params_joint_mle <- function(responses, design, A, K, sigma_y) {

  if (any(is.na(responses))) {
    return(list(alpha = NA_real_, w0 = NA_real_, w1 = NA_real_, converged = FALSE))
  }

  log_likelihood_joint <- function(params) {
    alpha <- params[1]
    w0 <- params[2]
    w1 <- params[3]

    if (alpha <= 0.001 || alpha >= 0.999) return(1e10)
    if (w1 <= 0) return(1e10)

    n_trials <- nrow(design)
    v_plus <- 0
    v_minus <- 0
    total_neg_log_lik <- 0

    for (t in seq_len(n_trials)) {
      g <- design$cs_plus[t] * v_plus + design$cs_minus[t] * v_minus
      theta <- A + (K - A) / (1 + exp(-(w0 + w1 * g)))
      total_neg_log_lik <- total_neg_log_lik - dnorm(responses[t], theta, sigma_y, log = TRUE)

      if (design$cs_plus[t] == 1) {
        v_plus <- v_plus + alpha * (design$us_plus[t] - v_plus)
      }
      if (design$cs_minus[t] == 1) {
        v_minus <- v_minus + alpha * (-1 - v_minus)
      }
    }

    total_neg_log_lik
  }

  # Multiple starting points for robust optimization
  start_points <- list(
    c(0.3, -1, 4),
    c(0.5, 0, 5),
    c(0.7, -2, 3)
  )

  best_result <- NULL
  best_value <- Inf

  for (start in start_points) {
    opt_result <- tryCatch({
      optim(
        par = start,
        fn = log_likelihood_joint,
        method = "L-BFGS-B",
        lower = c(0.001, -5, 0.5),
        upper = c(0.999, 4, 15)
      )
    }, error = function(e) {
      list(par = c(NA, NA, NA), value = Inf, convergence = 1)
    })

    if (opt_result$value < best_value) {
      best_value <- opt_result$value
      best_result <- opt_result
    }
  }

  converged <- !is.null(best_result) && best_result$convergence == 0

  list(
    alpha = best_result$par[1],
    w0 = best_result$par[2],
    w1 = best_result$par[3],
    converged = converged
  )
}


# ================================================================================
# 2. Participant Generation (UNIFORM ALPHA — matching main analysis distributions)
# ================================================================================

generate_uniform_participants <- function(n_participants = 100) {
  alpha <- runif(n_participants, min = 0.05, max = 0.95)
  w0 <- pmin(pmax(rnorm(n_participants, mean = -1, sd = 1.5), -5), 4)
  w1 <- runif(n_participants, min = 2, max = 8)

  data.frame(
    participant_id = seq_len(n_participants),
    alpha = alpha,
    w0 = w0,
    w1 = w1
  )
}


# ================================================================================
# 3. Identifiability Metrics
# ================================================================================

compute_identifiability_metrics <- function(alpha_true, alpha_hat, n_bootstrap = 1000) {

  valid_idx <- complete.cases(alpha_true, alpha_hat)
  alpha_true <- alpha_true[valid_idx]
  alpha_hat <- alpha_hat[valid_idx]

  n_valid <- length(alpha_true)

  if (n_valid < 3) {
    return(list(
      R2 = NA_real_,
      R2_ci_lower = NA_real_,
      R2_ci_upper = NA_real_,
      pearson_r = NA_real_,
      spearman_rho = NA_real_,
      RMSE = NA_real_,
      relative_RMSE = NA_real_,
      MAE = NA_real_,
      bias = NA_real_,
      n_valid = n_valid,
      convergence_rate = n_valid / length(valid_idx)
    ))
  }

  pearson_r <- cor(alpha_true, alpha_hat, method = "pearson")
  R2 <- pearson_r^2

  R2_boot <- numeric(n_bootstrap)
  for (b in 1:n_bootstrap) {
    boot_idx <- sample(seq_len(n_valid), n_valid, replace = TRUE)
    R2_boot[b] <- cor(alpha_true[boot_idx], alpha_hat[boot_idx])^2
  }
  R2_ci <- quantile(R2_boot, c(0.025, 0.975), na.rm = TRUE)

  spearman_rho <- cor(alpha_true, alpha_hat, method = "spearman")

  errors <- alpha_hat - alpha_true
  RMSE <- sqrt(mean(errors^2))
  MAE <- mean(abs(errors))
  bias <- mean(errors)

  relative_RMSE <- RMSE / sd(alpha_true)

  list(
    R2 = R2,
    R2_ci_lower = R2_ci[1],
    R2_ci_upper = R2_ci[2],
    pearson_r = pearson_r,
    spearman_rho = spearman_rho,
    RMSE = RMSE,
    relative_RMSE = relative_RMSE,
    MAE = MAE,
    bias = bias,
    n_valid = n_valid,
    convergence_rate = n_valid / length(valid_idx)
  )
}


# ================================================================================
# 4. Main Simulation Function (WITH PARALLEL & PROGRESS BAR)
# ================================================================================

run_identifiability_simulation <- function(
    n_participants = 100,
    parallel = TRUE,
    n_workers = NULL,
    show_progress = TRUE,
    sigma_y = 5,
    A_param = 0,
    K_param = 100,
    checkpoint_freq = 25,
    output_prefix = "alpha_identifiability"
) {
  cat("Running identifiability simulation...\n")

  # Generate factorial design space
  design_configs <- expand.grid(
    trials = c(10, 20, 30, 40, 60, 70, 80),
    rate = c(0.20, 0.30, 0.40, 0.55, 0.70, 0.85, 1.00),
    sigma_y = sigma_y,
    stringsAsFactors = FALSE
  )

  design_configs$ordering <- "random"
  design_configs$ratio <- "1:1"
  design_configs$design_id <- seq_len(nrow(design_configs))

  cat("  ", nrow(design_configs), "design configurations x", n_participants, "participants\n")

  # Generate synthetic participants
  participants <- generate_uniform_participants(
    n_participants = n_participants
  )

  # Set up parallel processing
  if (is.null(n_workers)) {
    n_workers <- future::availableCores() - 1
  }
  n_workers <- max(1, n_workers)

  previous_plan <- future::plan()
  if (parallel && n_workers > 1) {
    future::plan(future::multisession, workers = n_workers)
  } else {
    future::plan(future::sequential)
  }
  on.exit(future::plan(previous_plan), add = TRUE)

  if (show_progress) {
    progressr::handlers(global = TRUE)
    progressr::handlers("txtprogressbar")
  }

  simulate_one_design <- function(design_row, progress = NULL) {

    if (!is.null(progress)) {
      progress(sprintf("Design %d/%d", design_row$design_id, nrow(design_configs)))
    }

    n_trials <- design_row$trials
    rate <- design_row$rate
    sigma_y_design <- design_row$sigma_y
    ordering <- design_row$ordering
    ratio <- design_row$ratio

    results <- data.frame(
      design_id = design_row$design_id,
      participant_id = participants$participant_id,
      alpha_true = participants$alpha,
      alpha_hat = NA_real_,
      converged = FALSE
    )

    for (i in seq_len(n_participants)) {
      design <- create_experimental_design(n_trials, rate, ordering, ratio)
      responses <- simulate_conditioning_session(
        design,
        alpha = participants$alpha[i],
        w0 = participants$w0[i],
        w1 = participants$w1[i],
        A = A_param,
        K = K_param,
        sigma_y = sigma_y_design
      )

      recovery <- recover_params_joint_mle(
        responses,
        design,
        A = A_param,
        K = K_param,
        sigma_y = sigma_y_design
      )

      results$alpha_hat[i] <- recovery$alpha
      results$converged[i] <- recovery$converged
    }

    metrics <- compute_identifiability_metrics(
      results$alpha_true,
      results$alpha_hat
    )

    data.frame(
      design_id = design_row$design_id,
      trials = n_trials,
      rate = rate,
      sigma_y = sigma_y_design,
      ordering = ordering,
      ratio = ratio,
      R2 = metrics$R2,
      R2_ci_lower = metrics$R2_ci_lower,
      R2_ci_upper = metrics$R2_ci_upper,
      pearson_r = metrics$pearson_r,
      spearman_rho = metrics$spearman_rho,
      RMSE = metrics$RMSE,
      relative_RMSE = metrics$relative_RMSE,
      MAE = metrics$MAE,
      bias = metrics$bias,
      n_valid = metrics$n_valid,
      convergence_rate = metrics$convergence_rate,
      stringsAsFactors = FALSE
    )
  }

  # Run simulation across all designs
  start_time <- Sys.time()

  if (show_progress) {
    results_list <- progressr::with_progress({
      p <- progressr::progressor(steps = nrow(design_configs))
      future.apply::future_lapply(
        seq_len(nrow(design_configs)),
        function(i) simulate_one_design(design_configs[i, ], progress = p),
        future.seed = TRUE
      )
    })
  } else {
    results_list <- future.apply::future_lapply(
      seq_len(nrow(design_configs)),
      function(i) simulate_one_design(design_configs[i, ], progress = NULL),
      future.seed = TRUE
    )
  }

  results_summary <- do.call(rbind, results_list)

  end_time <- Sys.time()
  elapsed <- as.numeric(difftime(end_time, start_time, units = "mins"))
  cat("  Simulation complete (", round(elapsed, 2), "min)\n")

  # Save results
  output_data <- paste0(output_prefix, "_results.RData")
  output_csv <- paste0(output_prefix, "_summary.csv")

  save(
    results_summary,
    design_configs,
    participants,
    file = output_data
  )

  write.csv(results_summary, output_csv, row.names = FALSE)
  cat("  Saved:", output_data, "and", output_csv, "\n")

  # Summary statistics
  cat("  R2 range: [", round(min(results_summary$R2, na.rm = TRUE), 3), ",",
      round(max(results_summary$R2, na.rm = TRUE), 3), "]\n")

  return(results_summary)
}


# ================================================================================
# 5. Visualization Functions
# ================================================================================

visualize_identifiability_heatmap <- function(results_summary,
                                              output_file = "alpha_identifiability_heatmap") {
  cat("Creating identifiability heatmap...\n")

  optimal_idx <- which.max(results_summary$R2)
  optimal_design <- results_summary[optimal_idx, ]

  # Filter to sigma_y = 1 for the main figure (matching manuscript Supp Fig 3)
  plot_data <- results_summary %>%
    filter(sigma_y == 5) %>%
    mutate(
      trials_factor = factor(trials, levels = sort(unique(trials))),
      rate_factor = factor(rate, levels = sort(unique(rate)))
    )

  # Create faceted heatmap
  p_main <- ggplot(plot_data, aes(x = trials_factor, y = rate_factor, fill = R2)) +

    geom_tile(color = "white", linewidth = 1.5) +
    geom_text(
      aes(label = sprintf("%.2f", R2)),
      color = ifelse(plot_data$R2 < 0.5, "white", "gray20"),
      fontface = "bold",
      size = 4.5
    ) +

    scale_fill_gradient2(
      low = "#D32F2F",
      mid = "#FF9800",
      high = "#4CAF50",
      midpoint = 0.5,
      limits = c(0, 1),
      breaks = seq(0, 1, 0.2),
      labels = sprintf("%.1f", seq(0, 1, 0.2)),
      name = "R²"
    ) +

    labs(
      x = "Number of trials",
      y = "Reinforcement rate"
    ) +

    theme_minimal(base_size = 18) +
    theme(
      plot.title = element_blank(),
      plot.subtitle = element_blank(),
      strip.text = element_text(size = 20, face = "bold", color = "gray20"),
      strip.background = element_rect(fill = "gray95", color = NA),
      axis.title = element_text(size = 22, face = "bold", color = "gray20"),
      axis.text = element_text(size = 18, color = "gray30"),
      legend.position = "right",
      legend.key.height = unit(2.5, "cm"),
      legend.key.width = unit(1.0, "cm"),
      legend.title = element_text(size = 20, face = "bold"),
      legend.text = element_text(size = 18),
      panel.grid = element_blank(),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      plot.margin = margin(15, 15, 15, 15)
    ) +
    coord_fixed()

  ggsave(
    paste0(output_file, ".png"),
    p_main,
    width = 10,
    height = 9,
    dpi = 600,
    bg = "white"
  )

  ggsave(
    paste0(output_file, ".pdf"),
    p_main,
    width = 10,
    height = 9,
    bg = "white"
  )

  cat("  Saved:", paste0(output_file, ".png/.pdf"), "\n")

  return(p_main)
}


# ================================================================================
# 6. Descriptive Design Summary
# ================================================================================

analyze_design_effects <- function(results_summary,
                                  output_file = "alpha_identifiability_design_summary") {
  cat("Running descriptive design summary...\n")

  results_summary <- results_summary %>%
    filter(sigma_y == 5) %>%
    arrange(trials, rate)

  if (nrow(results_summary) == 0) {
    stop("No identifiability results available for sigma_y == 5.")
  }

  if (anyDuplicated(results_summary[c("trials", "rate")]) > 0) {
    stop("Expected one summary row per trials x rate cell.")
  }

  overall_summary <- tibble(
    n_cells = nrow(results_summary),
    n_trial_levels = n_distinct(results_summary$trials),
    n_rate_levels = n_distinct(results_summary$rate),
    mean_R2 = mean(results_summary$R2),
    median_R2 = median(results_summary$R2),
    sd_R2 = sd(results_summary$R2),
    min_R2 = min(results_summary$R2),
    max_R2 = max(results_summary$R2),
    note = paste(
      "Descriptive summary of the balanced trials x reinforcement grid.",
      "Because there is one R2 value per design cell, no residual degrees of freedom",
      "exist for valid F-tests or p-values."
    )
  )

  extrema <- bind_rows(
    results_summary %>%
      slice_max(R2, n = 1, with_ties = FALSE) %>%
      mutate(extremum = "max"),
    results_summary %>%
      slice_min(R2, n = 1, with_ties = FALSE) %>%
      mutate(extremum = "min")
  ) %>%
    select(
      extremum, trials, rate, R2, R2_ci_lower, R2_ci_upper,
      pearson_r, spearman_rho, RMSE, MAE, bias, n_valid, convergence_rate
    )

  trial_summary <- results_summary %>%
    group_by(trials) %>%
    summarise(
      mean_R2 = mean(R2),
      min_R2 = min(R2),
      max_R2 = max(R2),
      range_R2 = max_R2 - min_R2,
      .groups = "drop"
    ) %>%
    arrange(trials)

  rate_summary <- results_summary %>%
    group_by(rate) %>%
    summarise(
      mean_R2 = mean(R2),
      min_R2 = min(R2),
      max_R2 = max(R2),
      range_R2 = max_R2 - min_R2,
      .groups = "drop"
    ) %>%
    arrange(rate)

  grand_mean <- overall_summary$mean_R2[[1]]
  ss_total <- sum((results_summary$R2 - grand_mean) ^ 2)

  if (ss_total > 0) {
    ss_trials <- nrow(rate_summary) * sum((trial_summary$mean_R2 - grand_mean) ^ 2)
    ss_rate <- nrow(trial_summary) * sum((rate_summary$mean_R2 - grand_mean) ^ 2)
    ss_interaction <- max(ss_total - ss_trials - ss_rate, 0)
  } else {
    ss_trials <- 0
    ss_rate <- 0
    ss_interaction <- 0
  }

  decomposition <- tibble(
    term = c("trials", "rate", "trials:rate"),
    sum_sq = c(ss_trials, ss_rate, ss_interaction),
    proportion_of_surface_variation = if (ss_total > 0) c(ss_trials, ss_rate, ss_interaction) / ss_total else 0,
    interpretation = c(
      "Average differences across trial counts",
      "Average differences across reinforcement rates",
      "Remaining non-additive structure in the 49-cell R2 surface"
    )
  )

  write.csv(overall_summary, paste0(output_file, "_overall.csv"), row.names = FALSE)
  write.csv(extrema, paste0(output_file, "_extrema.csv"), row.names = FALSE)
  write.csv(trial_summary, paste0(output_file, "_trials.csv"), row.names = FALSE)
  write.csv(rate_summary, paste0(output_file, "_rates.csv"), row.names = FALSE)
  write.csv(decomposition, paste0(output_file, "_decomposition.csv"), row.names = FALSE)

  cat("  Saved:", paste0(output_file, "_overall.csv"), "\n")
  cat("  Saved:", paste0(output_file, "_extrema.csv"), "\n")
  cat("  Saved:", paste0(output_file, "_trials.csv"), "\n")
  cat("  Saved:", paste0(output_file, "_rates.csv"), "\n")
  cat("  Saved:", paste0(output_file, "_decomposition.csv"), "\n")

  cat(sprintf(
    "  Best cell   : %d trials, %.0f%% reinforcement (R2 = %.3f)\n",
    extrema$trials[extrema$extremum == "max"],
    100 * extrema$rate[extrema$extremum == "max"],
    extrema$R2[extrema$extremum == "max"]
  ))
  cat(sprintf(
    "  Worst cell  : %d trials, %.0f%% reinforcement (R2 = %.3f)\n",
    extrema$trials[extrema$extremum == "min"],
    100 * extrema$rate[extrema$extremum == "min"],
    extrema$R2[extrema$extremum == "min"]
  ))

  for (i in seq_len(nrow(decomposition))) {
    cat(sprintf(
      "  %-12s: %5.1f%% of surface variation\n",
      decomposition$term[i],
      100 * decomposition$proportion_of_surface_variation[i]
    ))
  }

  cat("\n")

  invisible(list(
    overall = overall_summary,
    extrema = extrema,
    trials = trial_summary,
    rates = rate_summary,
    decomposition = decomposition
  ))
}


# ================================================================================
# 7. Main Execution
# ================================================================================

main <- function() {
  results <- run_identifiability_simulation(
    n_participants = 100,
    parallel = TRUE,
    n_workers = NULL,
    show_progress = TRUE,
    sigma_y = 5,
    A_param = 0,
    K_param = 100,
    output_prefix = "alpha_identifiability"
  )

  plot <- visualize_identifiability_heatmap(
    results,
    output_file = "alpha_identifiability_heatmap"
  )

  design_summary <- analyze_design_effects(
    results,
    output_file = "alpha_identifiability_design_summary"
  )

  cat("All analyses complete!\n")

  invisible(list(
    results = results,
    plot = plot,
    design_summary = design_summary
  ))
}


# ================================================================================
# Run if not in interactive mode
# ================================================================================

if (!interactive()) {
  results <- main()
}
