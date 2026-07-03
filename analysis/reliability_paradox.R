# Reliability-paradox Monte Carlo: the rate-only test-retest simulation, the reliability heatmaps, and the long-protocol check (Fig 3).
# Writes the panel caches (panel_paradox_traj.rds, panel_paradox_icc.rds) that assemble_paradox_figures.R combines.

library(tidyverse)
library(patchwork)
library(future.apply)
library(progressr)

res_dir <- file.path(dirname(getwd()), "results")
fig_dir <- file.path(dirname(getwd()), "figures")

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

# fixed response-function parameters isolate alpha identification
W0_FIXED <- -1
W1_FIXED <- 4

generate_participants <- function(n = 200) {
  data.frame(
    alpha = runif(n, 0.05, 0.95),
    w0 = rep(W0_FIXED, n),
    w1 = rep(W1_FIXED, n)
  )
}

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

# one session design with specific non-reinforced CS+ positions
make_one_design <- function(n_trials, nonreinf_csplus_pos = NULL, n_shocks = NULL) {
  cs_sequence <- sample(rep(c(1, 0), each = n_trials / 2))
  cs_plus_idx <- which(cs_sequence == 1)
  n_csplus <- length(cs_plus_idx)
  us <- rep(0, n_trials)
  if (!is.null(nonreinf_csplus_pos)) {
    reinf_pos <- setdiff(seq_len(n_csplus), nonreinf_csplus_pos)
    us[cs_plus_idx[reinf_pos]] <- 1
  } else {
    us[sample(cs_plus_idx, n_shocks)] <- 1
  }
  data.frame(cs_plus = cs_sequence, cs_minus = 1 - cs_sequence, us_plus = us)
}

# C1 random both sessions; C2 non-reinforced early vs late; C3 end-clustered vs mid-scattered
create_experimental_designs <- function(n_trials = 20,
                                        n_cs_plus = 10,
                                        n_shocks = 8,
                                        manipulation = 'random_sequences') {
  n_nonreinf <- n_cs_plus - n_shocks

  if (manipulation == 'early_vs_late') {
    design_A <- make_one_design(n_trials, nonreinf_csplus_pos = seq_len(n_nonreinf))
    design_B <- make_one_design(n_trials, nonreinf_csplus_pos = (n_cs_plus - n_nonreinf + 1):n_cs_plus)

  } else if (manipulation == 'clustered_vs_scattered') {
    # end-clustered vs mid-scattered, no overlap
    end_pos <- (n_cs_plus - n_nonreinf + 1):n_cs_plus
    mid <- floor(n_cs_plus / 2)
    scattered_pos <- round(seq(2, mid, length.out = n_nonreinf))
    design_A <- make_one_design(n_trials, nonreinf_csplus_pos = end_pos)
    design_B <- make_one_design(n_trials, nonreinf_csplus_pos = scattered_pos)

  } else {
    design_A <- make_one_design(n_trials, n_shocks = n_shocks)
    design_B <- make_one_design(n_trials, n_shocks = n_shocks)
  }

  list(A = design_A, B = design_B)
}

calculate_behavioral_metrics <- function(responses, cs_plus_indicator) {
  valid_indices <- complete.cases(responses, cs_plus_indicator)
  responses <- responses[valid_indices]
  cs_plus_indicator <- cs_plus_indicator[valid_indices]
  n_trials <- length(responses)

  if (n_trials == 0) {
    return(list(slope = NA_real_, delta_first_last = NA_real_, last_trial = NA_real_))
  }

  metrics <- list()

  # CS+ only
  cs_plus_resp <- responses[cs_plus_indicator == 1]
  n_csplus <- length(cs_plus_resp)

  if (n_csplus > 2) {
    slope_model <- lm(cs_plus_resp ~ seq_len(n_csplus))
    metrics$slope <- unname(coef(slope_model)[2])
  } else {
    metrics$slope <- NA_real_
  }
  if (n_csplus >= 2) {
    metrics$delta_first_last <- cs_plus_resp[n_csplus] - cs_plus_resp[1]
    metrics$last_trial <- cs_plus_resp[n_csplus]
  } else {
    metrics$delta_first_last <- NA_real_
    metrics$last_trial <- NA_real_
  }

  metrics
}

# MLE recovery, oracle w0/w1
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

# joint MLE over (alpha, w0, w1)
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

# Monte Carlo reliability. "bootstrap" names are historical; each iteration draws fresh participants + sessions.
run_pure_simulation_bootstrap <- function(n_participants = 200,
                                          n_bootstrap = 200,
                                          n_trials = 20,
                                          n_cs_plus = 10,
                                          n_shocks = 8,
                                          manipulation_types = c('random_sequences',
                                                                'early_vs_late',
                                                                'clustered_vs_scattered'),
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
        manipulation == 'clustered_vs_scattered' ~ 'C3'
      ),
      condition_order = case_when(
        manipulation == 'random_sequences' ~ 1,
        manipulation == 'early_vs_late' ~ 2,
        manipulation == 'clustered_vs_scattered' ~ 3
      ),
      x_label = paste0(condition_num),
      metric_label = case_when(
        metric == 'slope' ~ 'Slope',
        metric == 'delta_first_last' ~ 'First-to-last\nchange',
        metric == 'last_trial' ~ 'Final response'
      ),
      metric_label = factor(metric_label,
                            levels = c('Slope', 'First-to-last\nchange', 'Final response'))
    )

  # learning-rate reliability (joint MLE)
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
        manipulation == 'clustered_vs_scattered' ~ 'C3'
      ),
      condition_order = case_when(
        manipulation == 'random_sequences' ~ 1,
        manipulation == 'early_vs_late' ~ 2,
        manipulation == 'clustered_vs_scattered' ~ 3
      ),
      x_label = paste0(condition_num),
      metric_label = 'Learning rate',
      metric_label = factor(metric_label, levels = c('Learning rate'))
    )

  build_heatmap <- function(data, panel_letter, panel_label, add_footer = FALSE) {
    data$x_label <- factor(data$x_label,
                           levels = unique(data$x_label[order(data$protocol_order,
                                                              data$condition_order)]))

    p <- ggplot(data, aes(x = x_label, y = metric_label, fill = mean_r)) +
      geom_tile(color = 'white', linewidth = 1.2) +
      geom_text(aes(label = sprintf('%.2f', mean_r)),
                size = 3.5, fontface = 'bold',
                color = ifelse(data$mean_r > 0.7, 'white', 'black')) +
      facet_grid(. ~ protocol, scales = 'free_x', space = 'free_x') +
      scale_fill_gradient2(
        low = '#D55E00', mid = '#F0E442', high = '#0072B2',
        midpoint = 0.5, limits = c(-0.25, 1.0),
        name = 'Test-retest\nreliability',
        breaks = c(-0.25, 0, 0.25, 0.5, 0.75, 1.0),
        labels = c('-0.25', '0', '0.25', '0.5', '0.75', '1.0')
      ) +
      scale_y_discrete(limits = rev) +
      labs(
        x = NULL,
        y = NULL,
        tag = paste0(panel_letter, '  ', panel_label)
      ) +
      theme_minimal(base_size = 11, base_family = 'Helvetica') +
      theme(
        plot.tag = element_text(face = 'bold', size = 12, hjust = 0),
        plot.tag.position = c(0, 1.05),
        strip.text = element_text(size = 10, face = 'bold', margin = margin(b = 4)),
        axis.text.x = element_text(size = 9, margin = margin(t = 2)),
        axis.text.y = element_text(size = 9, margin = margin(r = 3)),
        panel.grid = element_blank(),
        panel.spacing = unit(0.6, 'lines'),
        plot.margin = margin(t = 20, r = 5, b = 5, l = 5),
        legend.title = element_text(size = 9, face = 'bold'),
        legend.text = element_text(size = 8),
        legend.key.height = unit(0.6, 'cm'),
        legend.key.width = unit(0.4, 'cm')
      )

    if (add_footer) {
      p <- p + labs(caption = 'C1 = Random    C2 = Early vs. late    C3 = Clustered vs. scattered') +
        theme(
          plot.caption = element_text(size = 8, color = 'grey40', hjust = 0.5,
                                      margin = margin(t = 8))
        )
    }

    p
  }

  panel_cog   <- build_heatmap(cognitive_data, 'c', 'Model-based learning rate', add_footer = TRUE)
  panel_behav <- build_heatmap(behavioral_data, 'b', 'Behavioral summaries')

  fig <- (panel_behav / panel_cog) +
    plot_layout(heights = c(3, 1), guides = 'collect') &
    theme(legend.position = 'right')

  fig
}

# Monte Carlo (slow; cache-guarded)
cache_file <- file.path(res_dir, 'pure_simulation_results.RData')
if (file.exists(cache_file)) {
  message('Loading cached Monte Carlo results (skipping slow simulation).')
  load(cache_file)  # restores all_bootstrap_results
} else {
  # 20-trial protocol
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

  # 40-trial protocol
  results_standard <- run_pure_simulation_bootstrap(
    n_participants = 200,
    n_bootstrap = 200,
    n_trials = 40,
    n_cs_plus = 20,
    n_shocks = 16,  # 80% reinforcement
    A_param = 0,
    K_param = 100,
    sigma_y = 5,
    parallel = TRUE,
    show_progress = TRUE
  )

  all_bootstrap_results <- c(results_short, results_standard)

  save(all_bootstrap_results, file = cache_file)
}


# figures (fast)
summary_data <- summarize_results(all_bootstrap_results)

summary_print <- summary_data %>%
  mutate(
    protocol = ifelse(n_trials == 20, 'Short (20)', 'Standard (40)'),
    ci = sprintf('[%.3f, %.3f]', ci_lower, ci_upper)
  ) %>%
  select(protocol, manipulation, metric, mean_r, sd_r, ci)
print(as.data.frame(summary_print), row.names = FALSE)

# two-session trajectory data for panel a
set.seed(42)
n_traj <- 200
traj_parts <- generate_participants(n_traj)
traj_rows <- list()
idx <- 0
for (i in seq_len(n_traj)) {
  for (sess in c("Session 1", "Session 2")) {
    des <- create_experimental_designs(
      n_trials = 20, n_cs_plus = 10, n_shocks = 8,
      manipulation = 'random_sequences'
    )[['A']]
    resp <- simulate_conditioning_session(
      des, traj_parts$alpha[i], traj_parts$w0[i], traj_parts$w1[i],
      0, 100, 5
    )
    cs_plus_idx <- which(des$cs_plus == 1)
    for (j in seq_along(cs_plus_idx)) {
      idx <- idx + 1
      traj_rows[[idx]] <- data.frame(
        participant = i, session = sess,
        cs_trial = j, response = resp[cs_plus_idx[j]]
      )
    }
  }
}
traj_df <- do.call(rbind, traj_rows)

group_means <- aggregate(response ~ session + cs_trial, data = traj_df, FUN = mean)

# subsample individuals for readability
display_ids <- sample(seq_len(n_traj), 30)
indiv_df <- traj_df[traj_df$participant %in% display_ids, ]

# panel a: group-level stability
sess_colors <- c("Session 1" = "#2166AC", "Session 2" = "#B2182B")
panel_traj <- ggplot() +
  geom_line(data = indiv_df,
            aes(x = cs_trial, y = response, group = interaction(participant, session),
                color = session), alpha = 0.08, linewidth = 0.3) +
  geom_line(data = group_means,
            aes(x = cs_trial, y = response, color = session),
            linewidth = 1.3) +
  geom_point(data = group_means,
             aes(x = cs_trial, y = response, color = session),
             size = 2) +
  scale_color_manual(values = sess_colors, name = NULL) +
  scale_x_continuous(breaks = 1:10) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 25)) +
  labs(x = "CS+ trial", y = "Response (0-100)", tag = "a") +
  theme_minimal(base_size = 10) +
  theme(
    legend.position = "bottom",
    legend.key.height = unit(0.4, "cm"),
    legend.text = element_text(size = 8),
    legend.margin = margin(t = -2, b = 0),
    panel.grid.minor = element_blank(),
    plot.tag = element_text(face = "bold", size = 12)
  )

fig_icc <- create_figure_1(summary_data, trajectory_data = NULL)

fig_combined <- panel_traj / fig_icc + plot_layout(heights = c(1, 2.5))

ggsave(file.path(fig_dir, 'reliability_paradox.pdf'), fig_combined,
       width = 7, height = 7, bg = 'white', device = cairo_pdf, units = 'in')

# panels reused by assemble_paradox_figures.R
saveRDS(panel_traj, file.path(res_dir, 'panel_paradox_traj.rds'))
saveRDS(fig_icc,    file.path(res_dir, 'panel_paradox_icc.rds'))

write.csv(summary_data, file.path(res_dir, 'pure_simulation_summary.csv'), row.names = FALSE)

alpha_trt_mean <- mean(summary_data$mean_r[summary_data$metric == 'alpha_test_retest'])
alpha_rec_mean <- mean(summary_data$mean_r[summary_data$metric == 'alpha_recovery'])
behav_mean <- mean(summary_data$mean_r[summary_data$metric %in%
                                         c('slope', 'delta_first_last', 'last_trial')])
cat(sprintf('\nMean alpha TRT=%.3f, recovery=%.3f, behavioral=%.3f\n',
            alpha_trt_mean, alpha_rec_mean, behav_mean))
