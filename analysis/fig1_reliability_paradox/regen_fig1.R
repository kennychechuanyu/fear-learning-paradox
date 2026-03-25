#!/usr/bin/env Rscript
# Quick regeneration of Figure 1 with spaghetti panel
# Loads saved bootstrap results, generates trajectories, creates 3-panel figure

library(tidyverse)
library(patchwork)

# Set working directory
if (basename(getwd()) != "reliability_paradox") {
  if (dir.exists("proof of concept/reliability_paradox")) {
    setwd("proof of concept/reliability_paradox")
  }
}
cat("Working directory:", getwd(), "\n")

# ============================================================================
# Core functions
# ============================================================================

generate_participants <- function(n = 200) {
  data.frame(
    alpha = runif(n, 0.05, 0.95),
    w0 = pmin(pmax(rnorm(n, -1, 1.5), -5), 4),
    w1 = runif(n, 2, 8)
  )
}

simulate_conditioning_session <- function(design, alpha, w0, w1, A, K, sigma_y) {
  n_trials <- nrow(design)
  v_plus <- 0; v_minus <- 0
  responses <- numeric(n_trials)
  for (t in seq_len(n_trials)) {
    g <- design$cs_plus[t] * v_plus + design$cs_minus[t] * v_minus
    theta <- A + (K - A) / (1 + exp(-(w0 + w1 * g)))
    responses[t] <- rnorm(1, theta, sigma_y)
    if (design$cs_plus[t] == 1) v_plus <- v_plus + alpha * (design$us_plus[t] - v_plus)
    if (design$cs_minus[t] == 1) v_minus <- v_minus + alpha * (-1 - v_minus)
  }
  responses
}

create_experimental_designs <- function(n_trials = 20, n_cs_plus = 10,
                                        n_shocks = 8, manipulation = 'random_sequences') {
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
  list(A = design_A, B = design_B)
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
          condition = condition_name, n_trials = result$n_trials,
          manipulation = result$manipulation, metric = metric,
          mean_r = mean(values), median_r = median(values), sd_r = sd(values),
          ci_lower = quantile(values, 0.025), ci_upper = quantile(values, 0.975),
          n_bootstrap = length(values), stringsAsFactors = FALSE
        ))
      }
    }
  }
  summary_data
}

# ============================================================================
# Generate example trajectories for spaghetti panel
# ============================================================================

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
  cs_minus_label <- "CS\u2212"

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
      rows[[idx]] <- data.frame(participant = i, cs_type = "CS+",
                                cs_trial = j, response = responses[cs_plus_idx[j]])
    }
    for (j in seq_along(cs_minus_idx)) {
      idx <- idx + 1
      rows[[idx]] <- data.frame(participant = i, cs_type = cs_minus_label,
                                cs_trial = j, response = responses[cs_minus_idx[j]])
    }
  }
  all_data <- do.call(rbind, rows[seq_len(idx)])
  group_means <- all_data %>%
    group_by(cs_type, cs_trial) %>%
    summarize(mean_resp = mean(response), se_resp = sd(response) / sqrt(n()), .groups = "drop")
  display_ids <- sample(seq_len(n_participants), n_display)
  individual_data <- all_data %>% filter(participant %in% display_ids)
  list(group_means = group_means, individual_data = individual_data)
}

# ============================================================================
# Figure 1: 3-panel
# ============================================================================

create_figure_1 <- function(summary_data, trajectory_data = NULL) {
  cs_minus_label <- "CS\u2212"

  # Prepare behavioral heatmap data
  behavioral_data <- summary_data %>%
    filter(metric %in% c('slope', 'delta_first_last', 'last_trial')) %>%
    mutate(
      protocol = case_when(n_trials == 20 ~ '20 trials', n_trials == 40 ~ '40 trials', TRUE ~ as.character(n_trials)),
      protocol_order = case_when(n_trials == 20 ~ 1, n_trials == 40 ~ 2, TRUE ~ 99),
      condition_num = case_when(manipulation == 'random_sequences' ~ 'C1', manipulation == 'early_vs_late' ~ 'C2', manipulation == 'clustered_vs_distributed' ~ 'C3'),
      condition_order = case_when(manipulation == 'random_sequences' ~ 1, manipulation == 'early_vs_late' ~ 2, manipulation == 'clustered_vs_distributed' ~ 3),
      x_label = condition_num,
      metric_label = case_when(metric == 'slope' ~ 'Learning slope', metric == 'delta_first_last' ~ 'First-last\ndifference', metric == 'last_trial' ~ 'Last trial'),
      metric_label = factor(metric_label, levels = c('Learning slope', 'First-last\ndifference', 'Last trial'))
    )

  cognitive_data <- summary_data %>%
    filter(metric == 'alpha_free_trt') %>%
    mutate(
      protocol = case_when(n_trials == 20 ~ '20 trials', n_trials == 40 ~ '40 trials', TRUE ~ as.character(n_trials)),
      protocol_order = case_when(n_trials == 20 ~ 1, n_trials == 40 ~ 2, TRUE ~ 99),
      condition_num = case_when(manipulation == 'random_sequences' ~ 'C1', manipulation == 'early_vs_late' ~ 'C2', manipulation == 'clustered_vs_distributed' ~ 'C3'),
      condition_order = case_when(manipulation == 'random_sequences' ~ 1, manipulation == 'early_vs_late' ~ 2, manipulation == 'clustered_vs_distributed' ~ 3),
      x_label = condition_num,
      metric_label = factor('Learning rate', levels = 'Learning rate')
    )

  # Shared heatmap builder
  build_heatmap <- function(data, panel_letter, panel_label, add_footer = FALSE) {
    data$x_label <- factor(data$x_label,
                           levels = unique(data$x_label[order(data$protocol_order, data$condition_order)]))
    p <- ggplot(data, aes(x = x_label, y = metric_label, fill = mean_r)) +
      geom_tile(color = 'white', linewidth = 1.2) +
      geom_text(aes(label = sprintf('%.2f', mean_r)), size = 4.5, fontface = 'bold',
                color = ifelse(data$mean_r > 0.7, 'white', 'black')) +
      facet_grid(. ~ protocol, scales = 'free_x', space = 'free_x') +
      scale_fill_gradient2(low = '#D55E00', mid = '#F0E442', high = '#0072B2',
                           midpoint = 0.5, limits = c(-0.25, 1.0),
                           name = 'Test-retest\nICC(2,1)',
                           breaks = c(-0.25, 0, 0.25, 0.5, 0.75, 1.0),
                           labels = c('-0.25', '0', '0.25', '0.5', '0.75', '1.0')) +
      scale_y_discrete(limits = rev) +
      labs(x = NULL, y = NULL, tag = paste0(panel_letter, '  ', panel_label)) +
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
        theme(plot.caption = element_text(size = 10, color = 'grey40', hjust = 0.5, margin = margin(t = 12)))
    }
    p
  }

  # Build heatmap panels
  if (!is.null(trajectory_data)) {
    panel_behav <- build_heatmap(behavioral_data, 'b', 'Behavioral markers')
    panel_cog   <- build_heatmap(cognitive_data, 'c', 'Cognitive markers', add_footer = TRUE)
  } else {
    panel_behav <- build_heatmap(behavioral_data, 'a', 'Behavioral markers')
    panel_cog   <- build_heatmap(cognitive_data, 'b', 'Cognitive markers', add_footer = TRUE)
  }

  # Build spaghetti panel
  if (!is.null(trajectory_data)) {
    gm  <- trajectory_data$group_means
    ind <- trajectory_data$individual_data

    cs_plus_col  <- '#C62828'
    cs_minus_col <- '#1565C0'
    cs_levels <- c("CS+", cs_minus_label)
    gm$cs_type  <- factor(gm$cs_type, levels = cs_levels)
    ind$cs_type <- factor(ind$cs_type, levels = cs_levels)

    panel_spaghetti <- ggplot() +
      geom_line(data = ind,
                aes(x = cs_trial, y = response,
                    group = interaction(participant, cs_type), color = cs_type),
                alpha = 0.07, linewidth = 0.3, show.legend = FALSE) +
      geom_ribbon(data = gm,
                  aes(x = cs_trial, ymin = mean_resp - se_resp,
                      ymax = mean_resp + se_resp, fill = cs_type),
                  alpha = 0.2, show.legend = FALSE) +
      geom_line(data = gm, aes(x = cs_trial, y = mean_resp, color = cs_type),
                linewidth = 1.4) +
      geom_point(data = gm, aes(x = cs_trial, y = mean_resp, color = cs_type,
                                shape = cs_type), size = 2.2, stroke = 0.5) +
      scale_color_manual(values = setNames(c(cs_plus_col, cs_minus_col), cs_levels), name = NULL) +
      scale_fill_manual(values = setNames(c(cs_plus_col, cs_minus_col), cs_levels)) +
      scale_shape_manual(values = setNames(c(16, 17), cs_levels), name = NULL) +
      scale_x_continuous(breaks = c(1, 5, 10), expand = c(0.03, 0)) +
      scale_y_continuous(limits = c(-15, 115), breaks = seq(0, 100, 25)) +
      labs(x = 'Trial (within CS type)', y = 'Response', tag = 'a') +
      annotate("text", x = 0.5, y = 113,
               label = "Group-level learning is robust, but individuals vary widely",
               hjust = 0, size = 3.8, fontface = "bold", color = "gray25") +
      theme_minimal(base_size = 13, base_family = 'Helvetica') +
      theme(
        plot.tag = element_text(face = 'bold', size = 14),
        plot.tag.position = c(0, 1.02),
        legend.position = c(0.93, 0.45),
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

# ============================================================================
# Model Variance Panel (panel a): Two individuals, different alphas, similar means
# ============================================================================

create_model_variance_panel <- function() {
  # Simulate two exemplar participants
  simulate_traj <- function(alpha, n_trials = 30, shock_trials, w0 = -1, w1 = 4,
                            A = 0, K = 100, sigma = 0.5) {
    v <- 0; responses <- numeric(n_trials)
    for (t in seq_len(n_trials)) {
      responses[t] <- A + (K - A) / (1 + exp(-(w0 + w1 * v))) + rnorm(1, 0, sigma)
      if (t %in% shock_trials) v <- v + alpha * (1 - v)
      else v <- v + alpha * (0 - v)
    }
    data.frame(trial = seq_len(n_trials), response = responses,
               shock = seq_len(n_trials) %in% shock_trials)
  }

  set.seed(12345)
  shocks_A <- c(10, 12, 14, 16, 18, 20, 22, 24, 26, 28)
  shocks_B <- c(2, 4, 6, 8, 10, 12, 14, 16, 18, 20)

  dA <- simulate_traj(0.90, shock_trials = shocks_A)
  dB <- simulate_traj(0.10, shock_trials = shocks_B)

  dA$individual <- "Fast learner (\u03b1 = 0.90, late reinforcement)"
  dB$individual <- "Slow learner (\u03b1 = 0.10, early reinforcement)"
  dA$trial_type <- ifelse(dA$shock, "Shock", "No shock")
  dB$trial_type <- ifelse(dB$shock, "Shock", "No shock")

  all_data <- rbind(dA, dB)

  # Compute means for annotation
  mean_A <- round(mean(dA$response), 1)
  mean_B <- round(mean(dB$response), 1)
  fld_A <- round(dA$response[30] - dA$response[1], 1)
  fld_B <- round(dB$response[30] - dB$response[1], 1)

  ind_levels <- c(
    "Fast learner (\u03b1 = 0.90, late reinforcement)",
    "Slow learner (\u03b1 = 0.10, early reinforcement)"
  )
  all_data$individual <- factor(all_data$individual, levels = ind_levels)

  table_text <- sprintf(
    "Fast learner:  Mean = %s,  First-last = %s\nSlow learner:  Mean = %s,  First-last = %s",
    mean_A, fld_A, mean_B, fld_B
  )

  p <- ggplot(all_data, aes(x = trial, y = response, color = individual, group = individual)) +
    geom_line(linewidth = 0.9, alpha = 0.8) +
    geom_point(aes(shape = trial_type), size = 1.8, alpha = 0.9) +
    scale_shape_manual(values = c("Shock" = 17, "No shock" = 16), name = NULL) +
    scale_color_manual(values = setNames(c("#E63946", "#1565C0"), ind_levels), name = NULL) +
    # Annotation box
    annotate("rect", xmin = 11.5, xmax = 30.5, ymin = 2, ymax = 20,
             fill = "white", color = "gray50", linewidth = 0.4, alpha = 0.92) +
    annotate("text", x = 30, y = 11, label = table_text,
             hjust = 1, vjust = 0.5, size = 2.7, lineheight = 1.15, color = "black") +
    scale_x_continuous(breaks = c(1, 10, 20, 30)) +
    coord_cartesian(xlim = c(1, 30), ylim = c(0, 100)) +
    labs(x = "Trial", y = "Response", tag = "a") +
    annotate("text", x = 1, y = 99,
             label = "Different learning rates, similar behavioral summaries",
             hjust = 0, size = 3.5, fontface = "bold", color = "gray25") +
    theme_minimal(base_size = 13, base_family = "Helvetica") +
    theme(
      plot.tag = element_text(face = "bold", size = 14),
      plot.tag.position = c(0, 1.02),
      legend.position = "bottom",
      legend.text = element_text(size = 9),
      legend.key.size = unit(0.8, "lines"),
      legend.margin = margin(t = -5, b = 0),
      legend.box = "horizontal",
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray92", linewidth = 0.3),
      axis.title = element_text(size = 11, color = "gray30"),
      axis.text = element_text(size = 10, color = "gray40"),
      plot.margin = margin(t = 18, r = 10, b = 2, l = 5),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA)
    ) +
    guides(color = guide_legend(order = 1, nrow = 1),
           shape = guide_legend(order = 2, nrow = 1))

  p
}

# ============================================================================
# Run: 3-panel figure (model variance + heatmaps)
# ============================================================================

cat("Loading saved bootstrap results...\n")
load("pure_simulation_results.RData")
summary_data <- summarize_results(all_bootstrap_results)

cat("Creating model variance panel...\n")
panel_mv <- create_model_variance_panel()

cat("Creating heatmap panels...\n")
fig1 <- create_figure_1(summary_data, trajectory_data = NULL)
# We need to extract the two heatmap panels and combine with model variance
# Rebuild manually:

# Re-extract heatmap panels with correct labels (b, c)
# We call create_figure_1 with a special flag — simpler to just rebuild here

# Prepare behavioral data
behavioral_data <- summary_data %>%
  filter(metric %in% c('slope', 'delta_first_last', 'last_trial')) %>%
  mutate(
    protocol = case_when(n_trials == 20 ~ '20 trials', n_trials == 40 ~ '40 trials', TRUE ~ as.character(n_trials)),
    protocol_order = case_when(n_trials == 20 ~ 1, n_trials == 40 ~ 2, TRUE ~ 99),
    condition_num = case_when(manipulation == 'random_sequences' ~ 'C1', manipulation == 'early_vs_late' ~ 'C2', manipulation == 'clustered_vs_distributed' ~ 'C3'),
    condition_order = case_when(manipulation == 'random_sequences' ~ 1, manipulation == 'early_vs_late' ~ 2, manipulation == 'clustered_vs_distributed' ~ 3),
    x_label = condition_num,
    metric_label = case_when(metric == 'slope' ~ 'Learning slope', metric == 'delta_first_last' ~ 'First-last\ndifference', metric == 'last_trial' ~ 'Last trial'),
    metric_label = factor(metric_label, levels = c('Learning slope', 'First-last\ndifference', 'Last trial'))
  )

cognitive_data <- summary_data %>%
  filter(metric == 'alpha_free_trt') %>%
  mutate(
    protocol = case_when(n_trials == 20 ~ '20 trials', n_trials == 40 ~ '40 trials', TRUE ~ as.character(n_trials)),
    protocol_order = case_when(n_trials == 20 ~ 1, n_trials == 40 ~ 2, TRUE ~ 99),
    condition_num = case_when(manipulation == 'random_sequences' ~ 'C1', manipulation == 'early_vs_late' ~ 'C2', manipulation == 'clustered_vs_distributed' ~ 'C3'),
    condition_order = case_when(manipulation == 'random_sequences' ~ 1, manipulation == 'early_vs_late' ~ 2, manipulation == 'clustered_vs_distributed' ~ 3),
    x_label = condition_num,
    metric_label = factor('Learning rate', levels = 'Learning rate')
  )

build_heatmap <- function(data, panel_letter, panel_label, add_footer = FALSE) {
  data$x_label <- factor(data$x_label,
                         levels = unique(data$x_label[order(data$protocol_order, data$condition_order)]))
  p <- ggplot(data, aes(x = x_label, y = metric_label, fill = mean_r)) +
    geom_tile(color = 'white', linewidth = 1.2) +
    geom_text(aes(label = sprintf('%.2f', mean_r)), size = 4.5, fontface = 'bold',
              color = ifelse(data$mean_r > 0.7, 'white', 'black')) +
    facet_grid(. ~ protocol, scales = 'free_x', space = 'free_x') +
    scale_fill_gradient2(low = '#D55E00', mid = '#F0E442', high = '#0072B2',
                         midpoint = 0.5, limits = c(-0.25, 1.0),
                         name = 'Test-retest\nICC(2,1)',
                         breaks = c(-0.25, 0, 0.25, 0.5, 0.75, 1.0),
                         labels = c('-0.25', '0', '0.25', '0.5', '0.75', '1.0')) +
    scale_y_discrete(limits = rev) +
    labs(x = NULL, y = NULL, tag = paste0(panel_letter, '  ', panel_label)) +
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
      theme(plot.caption = element_text(size = 10, color = 'grey40', hjust = 0.5, margin = margin(t = 12)))
  }
  p
}

panel_b <- build_heatmap(behavioral_data, 'b', 'Behavioral markers')
panel_c <- build_heatmap(cognitive_data, 'c', 'Cognitive markers', add_footer = TRUE)

# Combine all three panels
fig1_combined <- (panel_mv / panel_b / panel_c) +
  plot_layout(heights = c(2.5, 3, 1.3)) &
  theme(plot.background = element_rect(fill = 'white', color = NA))

ggsave("fig1_reliability_paradox_pure.png", fig1_combined,
       width = 7.09, height = 8.5, dpi = 600, bg = "white", units = "in")
ggsave("fig1_reliability_paradox_pure.pdf", fig1_combined,
       width = 7.09, height = 8.5, bg = "white", device = cairo_pdf, units = "in")

cat("Done! Saved 3-panel fig1 (model variance + heatmaps)\n")
