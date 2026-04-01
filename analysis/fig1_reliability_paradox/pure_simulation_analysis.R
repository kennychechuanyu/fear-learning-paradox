# Pure simulation reliability paradox analysis
# Sections 1-7: function definitions. 8A: bootstrap (slow). 8B: figures (fast). 8C: extended protocol.

library(tidyverse)
library(patchwork)
library(future.apply)
library(progressr)

# ICC(2,1)
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

# Set working directory
if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  script_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
  if (script_dir != "" && basename(script_dir) == "reliability_paradox") {
    setwd(script_dir)
  }
} else if (basename(getwd()) != "reliability_paradox") {
  if (dir.exists("reliability_paradox")) {
    setwd("reliability_paradox")
  }
}

# --- 1. Parameter Generation ---

generate_participants <- function(n = 200) {
  data.frame(
    alpha = runif(n, 0.05, 0.95),
    w0 = pmin(pmax(rnorm(n, -1, 1.5), -5), 4),
    w1 = runif(n, 2, 8)
  )
}

# --- 2. Conditioning Session Simulation ---

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

# --- 3. Experimental Design Generation ---

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

# --- 4. Behavioral Metrics ---

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

# --- 5. Parameter Recovery (MLE) ---

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

# --- 5b. Parameter Recovery — Free Response Function (joint alpha, w0, w1) ---

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

# --- 6. Bootstrap Reliability Analysis ---

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

      # Draw fresh participants each iteration
      participants <- generate_participants(n_participants)

      # Storage
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

# --- 6b. Example Trajectories for Spaghetti Panel ---

generate_example_trajectories <- function(n_participants = 200,
                                          n_display = 30,
                                          n_trials = 20,
                                          n_cs_plus = 10,
                                          n_shocks = 8,
                                          A_param = 0,
                                          K_param = 100,
                                          sigma_y = 5) {
  set.seed(42)
  participants <- generate_participants(n_participants)
  rows <- vector("list", n_participants * n_trials)
  idx <- 0

  for (i in seq_len(n_participants)) {
    design <- create_experimental_designs(
      n_trials = n_trials, n_cs_plus = n_cs_plus,
      n_shocks = n_shocks, manipulation = 'random_sequences'
    )[['A']]

    responses <- simulate_conditioning_session(
      design, participants$alpha[i], participants$w0[i], participants$w1[i],
      A_param, K_param, sigma_y
    )

    cs_plus_idx <- which(design$cs_plus == 1)
    cs_minus_idx <- which(design$cs_minus == 1)

    for (j in seq_along(cs_plus_idx)) {
      idx <- idx + 1
      rows[[idx]] <- data.frame(
        participant = i, cs_type = "CS+",
        cs_trial = j, response = responses[cs_plus_idx[j]]
      )
    }
    for (j in seq_along(cs_minus_idx)) {
      idx <- idx + 1
      rows[[idx]] <- data.frame(
        participant = i, cs_type = "CS\u2212",
        cs_trial = j, response = responses[cs_minus_idx[j]]
      )
    }
  }

  all_data <- do.call(rbind, rows[seq_len(idx)])

  group_means <- all_data %>%
    group_by(cs_type, cs_trial) %>%
    summarize(mean_resp = mean(response),
              se_resp = sd(response) / sqrt(n()), .groups = "drop")

  display_ids <- sample(seq_len(n_participants), n_display)
  individual_data <- all_data %>% filter(participant %in% display_ids)

  list(group_means = group_means, individual_data = individual_data)
}

# --- 7. Summary and Visualization ---

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

create_figure_1 <- function(summary_data, trajectory_data = NULL) {
  # Create figure for Nature Neuroscience Brief Communication
  #
  # Panel a: Spaghetti plot — group-level learning is robust (optional)
  # Panel b: Behavioral metrics heatmap (3 rows x 6 columns)
  # Panel c: Learning rates heatmap (1 row x 6 columns)

  # Prepare behavioral data
  behavioral_data <- summary_data %>%
    filter(metric %in% c('slope', 'delta_first_last', 'last_trial')) %>%
    mutate(
      protocol = case_when(
        n_trials == 20 ~ '20 trials',
        n_trials == 40 ~ '40 trials',
        TRUE ~ as.character(n_trials)
      ),
      protocol_order = case_when(
        n_trials == 20 ~ 1,
        n_trials == 40 ~ 2,
        TRUE ~ 99
      ),
      condition_num = case_when(
        manipulation == 'random_sequences' ~ 'C1',
        manipulation == 'early_vs_late' ~ 'C2',
        manipulation == 'clustered_vs_distributed' ~ 'C3'
      ),
      condition_order = case_when(
        manipulation == 'random_sequences' ~ 1,
        manipulation == 'early_vs_late' ~ 2,
        manipulation == 'clustered_vs_distributed' ~ 3
      ),
      x_label = paste0(condition_num),
      metric_label = case_when(
        metric == 'slope' ~ 'Learning slope',
        metric == 'delta_first_last' ~ 'First-last\ndifference',
        metric == 'last_trial' ~ 'Last trial'
      ),
      metric_label = factor(metric_label,
                            levels = c('Learning slope', 'First-last\ndifference', 'Last trial'))
    )

  # Prepare cognitive data (joint estimation only)
  cognitive_data <- summary_data %>%
    filter(metric == 'alpha_free_trt') %>%
    mutate(
      protocol = case_when(
        n_trials == 20 ~ '20 trials',
        n_trials == 40 ~ '40 trials',
        TRUE ~ as.character(n_trials)
      ),
      protocol_order = case_when(
        n_trials == 20 ~ 1,
        n_trials == 40 ~ 2,
        TRUE ~ 99
      ),
      condition_num = case_when(
        manipulation == 'random_sequences' ~ 'C1',
        manipulation == 'early_vs_late' ~ 'C2',
        manipulation == 'clustered_vs_distributed' ~ 'C3'
      ),
      condition_order = case_when(
        manipulation == 'random_sequences' ~ 1,
        manipulation == 'early_vs_late' ~ 2,
        manipulation == 'clustered_vs_distributed' ~ 3
      ),
      x_label = paste0(condition_num),
      metric_label = 'Learning rate',
      metric_label = factor(metric_label, levels = c('Learning rate'))
    )

  # Shared heatmap builder
  build_heatmap <- function(data, panel_letter, panel_label, add_footer = FALSE) {
    # Create x-axis ordering
    data$x_label <- factor(data$x_label,
                           levels = unique(data$x_label[order(data$protocol_order,
                                                              data$condition_order)]))

    p <- ggplot(data, aes(x = x_label, y = metric_label, fill = mean_r)) +
      geom_tile(color = 'white', linewidth = 1.2) +
      geom_text(aes(label = sprintf('%.2f', mean_r)),
                size = 4.5, fontface = 'bold',
                color = ifelse(data$mean_r > 0.7, 'white', 'black')) +
      facet_grid(. ~ protocol, scales = 'free_x', space = 'free_x') +
      scale_fill_gradient2(
        low = '#D55E00', mid = '#F0E442', high = '#0072B2',
        midpoint = 0.5, limits = c(-0.25, 1.0),
        name = 'Test-retest\nICC(2,1)',
        breaks = c(-0.25, 0, 0.25, 0.5, 0.75, 1.0),
        labels = c('-0.25', '0', '0.25', '0.5', '0.75', '1.0')
      ) +
      scale_y_discrete(limits = rev) +
      labs(
        x = NULL,
        y = NULL,
        tag = paste0(panel_letter, '  ', panel_label)
      ) +
      theme_minimal(base_size = 13, base_family = 'Helvetica') +
      theme(
        plot.tag = element_text(face = 'bold', size = 14, hjust = 0),
        plot.tag.position = c(0, 1.05),
        strip.text = element_text(size = 12, face = 'bold', margin = margin(b = 8)),
        axis.text.x = element_text(size = 11, margin = margin(t = 3)),
        axis.text.y = element_text(size = 11, margin = margin(r = 5)),
        panel.grid = element_blank(),
        panel.spacing = unit(0.8, 'lines'),
        plot.margin = margin(t = 30, r = 5, b = 5, l = 5),
        legend.title = element_text(size = 11, face = 'bold'),
        legend.text = element_text(size = 10),
        legend.key.height = unit(0.8, 'cm'),
        legend.key.width = unit(0.5, 'cm')
      )

    if (add_footer) {
      p <- p + labs(caption = 'C1 = Random sequences    C2 = Early vs. late reinforcement    C3 = Clustered vs. distributed') +
        theme(
          plot.caption = element_text(size = 10, color = 'grey40', hjust = 0.5,
                                      margin = margin(t = 12))
        )
    }

    p
  }

  # Build heatmap panels
  if (!is.null(trajectory_data)) {
    panel_behav <- build_heatmap(behavioral_data, 'b', 'Behavioral metrics')
    panel_cog   <- build_heatmap(cognitive_data, 'c', 'Learning rates', add_footer = TRUE)
  } else {
    panel_behav <- build_heatmap(behavioral_data, 'a', 'Behavioral metrics')
    panel_cog   <- build_heatmap(cognitive_data, 'b', 'Learning rates', add_footer = TRUE)
  }

  if (!is.null(trajectory_data)) {
    gm  <- trajectory_data$group_means
    ind <- trajectory_data$individual_data

    cs_plus_col  <- '#C62828'
    cs_minus_col <- '#1565C0'

    panel_spaghetti <- ggplot() +
      # Individual trajectories
      geom_line(
        data = ind,
        aes(x = cs_trial, y = response,
            group = interaction(participant, cs_type), color = cs_type),
        alpha = 0.07, linewidth = 0.3, show.legend = FALSE
      ) +
      # Group mean ribbon (SEM)
      geom_ribbon(
        data = gm,
        aes(x = cs_trial,
            ymin = mean_resp - se_resp, ymax = mean_resp + se_resp,
            fill = cs_type),
        alpha = 0.2, show.legend = FALSE
      ) +
      # Group mean lines
      geom_line(
        data = gm,
        aes(x = cs_trial, y = mean_resp, color = cs_type),
        linewidth = 1.4
      ) +
      geom_point(
        data = gm,
        aes(x = cs_trial, y = mean_resp, color = cs_type, shape = cs_type),
        size = 2.2, stroke = 0.5
      ) +
      scale_color_manual(
        values = c("CS+" = cs_plus_col, "CS\u2212" = cs_minus_col),
        name = NULL
      ) +
      scale_fill_manual(
        values = c("CS+" = cs_plus_col, "CS\u2212" = cs_minus_col)
      ) +
      scale_shape_manual(
        values = c("CS+" = 16, "CS\u2212" = 17),
        name = NULL
      ) +
      scale_x_continuous(
        breaks = c(1, 5, 10),
        expand = c(0.03, 0)
      ) +
      scale_y_continuous(
        limits = c(-15, 115),
        breaks = seq(0, 100, 25)
      ) +
      labs(
        x = 'Trial (within CS type)',
        y = 'Response',
        tag = 'a'
      ) +
      annotate("text", x = 0.5, y = 112,
               label = "Group-level conditioning is robust",
               hjust = 0, size = 4.2, fontface = "bold", color = "gray25") +
      annotate("text", x = 0.5, y = 103,
               label = "Individual trajectories (thin lines) vary widely, yet the group mean (bold) shows clear learning",
               hjust = 0, size = 3.0, color = "gray45") +
      theme_minimal(base_size = 13, base_family = 'Helvetica') +
      theme(
        plot.tag = element_text(face = 'bold', size = 14),
        plot.tag.position = c(0, 1.02),
        legend.position = c(0.92, 0.5),
        legend.background = element_rect(fill = alpha('white', 0.85), color = NA),
        legend.text = element_text(size = 11),
        legend.key.size = unit(0.9, 'lines'),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = 'gray92', linewidth = 0.3),
        axis.title = element_text(size = 11, color = 'gray30'),
        axis.text = element_text(size = 10, color = 'gray40'),
        plot.margin = margin(t = 20, r = 10, b = 5, l = 5),
        plot.background = element_rect(fill = 'white', color = NA),
        panel.background = element_rect(fill = 'white', color = NA)
      )

    fig <- (panel_spaghetti / panel_behav / panel_cog) +
      plot_layout(heights = c(2.2, 3, 1.3), guides = 'collect') &
      theme(legend.position = 'right')
  } else {
    fig <- (panel_behav / panel_cog) +
      plot_layout(heights = c(3, 1.3), guides = 'collect') &
      theme(legend.position = 'right')
  }

  fig
}

# --- 8A. BOOTSTRAP EXECUTION (RUN ONCE - SLOW) ---

# Short protocol (20 trials)
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

# Standard protocol (40 trials)
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

# Combine results
all_bootstrap_results <- c(results_short, results_standard)

# Save results
save(all_bootstrap_results,
     file = 'pure_simulation_results.RData')


# --- 8B. VISUALIZATION (RUN AFTER LOADING SAVED RESULTS - FAST) ---

# Uncomment to load saved results:
# load('pure_simulation_results.RData')

# Summarize
summary_data <- summarize_results(all_bootstrap_results)

# Print summary table
summary_print <- summary_data %>%
  mutate(
    protocol = ifelse(n_trials == 20, 'Short (20)', 'Standard (40)'),
    ci = sprintf('[%.3f, %.3f]', ci_lower, ci_upper)
  ) %>%
  select(protocol, manipulation, metric, mean_r, sd_r, ci)
print(as.data.frame(summary_print), row.names = FALSE)

# Generate example trajectories for spaghetti panel
traj_data <- generate_example_trajectories(
  n_participants = 200, n_display = 30,
  n_trials = 20, n_cs_plus = 10, n_shocks = 8,
  A_param = 0, K_param = 100, sigma_y = 5
)

# Create figure (3-panel: spaghetti + behavioral heatmap + cognitive heatmap)
fig1 <- create_figure_1(summary_data, trajectory_data = traj_data)

# Save figure
ggsave('fig1_reliability_paradox_pure.png', fig1,
       width = 7.09, height = 8.5, dpi = 600, bg = 'white', units = 'in')

ggsave('fig1_reliability_paradox_pure.pdf', fig1,
       width = 7.09, height = 8.5, bg = 'white', device = cairo_pdf, units = 'in')

# Save summary CSV
write.csv(summary_data, 'pure_simulation_summary.csv', row.names = FALSE)

# Print overall summary
alpha_trt_mean <- mean(summary_data$mean_r[summary_data$metric == 'alpha_test_retest'])
alpha_rec_mean <- mean(summary_data$mean_r[summary_data$metric == 'alpha_recovery'])
behav_mean <- mean(summary_data$mean_r[summary_data$metric %in%
                                         c('slope', 'delta_first_last', 'last_trial')])
cat(sprintf('\nMean alpha TRT=%.3f, recovery=%.3f, behavioral=%.3f\n',
            alpha_trt_mean, alpha_rec_mean, behav_mean))

# --- 8C. 1,000-TRIAL BEHAVIORAL-ONLY SIMULATION ---
# Tests whether extended protocols improve behavioral reliability.
# 200 iterations, N = 200, 1000 trials (500 CS+, 500 CS-, 80% reinforcement).
# Behavioral metrics only — no model fitting required.

run_long_protocol_behavioral <- function(
    n_participants = 200,
    n_bootstrap    = 200,
    n_trials       = 1000,
    n_cs_plus      = 500,
    n_shocks       = 400,   # 80% reinforcement
    A_param        = 0,
    K_param        = 100,
    sigma_y        = 5
) {
  icc_slope <- numeric(n_bootstrap)
  icc_fl    <- numeric(n_bootstrap)
  icc_last  <- numeric(n_bootstrap)

  for (b in seq_len(n_bootstrap)) {
    participants <- generate_participants(n_participants)

    slope_A <- numeric(n_participants); slope_B <- numeric(n_participants)
    fl_A    <- numeric(n_participants); fl_B    <- numeric(n_participants)
    last_A  <- numeric(n_participants); last_B  <- numeric(n_participants)

    for (i in seq_len(n_participants)) {
      p       <- participants[i, ]
      designs <- create_experimental_designs(n_trials, n_cs_plus, n_shocks,
                                             manipulation = 'random_sequences')

      resp_A <- simulate_conditioning_session(designs$A, p$alpha, p$w0, p$w1,
                                              A_param, K_param, sigma_y)
      resp_B <- simulate_conditioning_session(designs$B, p$alpha, p$w0, p$w1,
                                              A_param, K_param, sigma_y)

      bm_A <- calculate_behavioral_metrics(resp_A, designs$A$cs_plus)
      bm_B <- calculate_behavioral_metrics(resp_B, designs$B$cs_plus)

      slope_A[i] <- bm_A$slope;            slope_B[i] <- bm_B$slope
      fl_A[i]    <- bm_A$delta_first_last; fl_B[i]    <- bm_B$delta_first_last
      last_A[i]  <- bm_A$last_trial;       last_B[i]  <- bm_B$last_trial
    }

    icc_slope[b] <- icc_2_1(slope_A, slope_B)
    icc_fl[b]    <- icc_2_1(fl_A,    fl_B)
    icc_last[b]  <- icc_2_1(last_A,  last_B)
  }

  mean_icc <- mean(c(icc_slope, icc_fl, icc_last), na.rm = TRUE)

  cat(sprintf('1000-trial behavioral ICCs: slope=%.2f, first-last=%.2f, last=%.2f, mean=%.2f\n',
              mean(icc_slope), mean(icc_fl), mean(icc_last), mean_icc))

  invisible(list(
    icc_slope = icc_slope,
    icc_fl    = icc_fl,
    icc_last  = icc_last,
    mean_icc  = mean_icc
  ))
}

results_1000 <- run_long_protocol_behavioral()
save(results_1000, file = 'long_protocol_behavioral_results.RData')
