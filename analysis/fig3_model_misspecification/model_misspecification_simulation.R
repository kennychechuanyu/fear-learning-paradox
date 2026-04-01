# Model misspecification simulation
# Compares recovery quality of correct (Pearce-Hall) vs. misspecified (RW) model


# Required packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,
  patchwork,
  scales
)

# Working directory setup
if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  script_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
  if (basename(script_dir) == "source 3") {
    setwd(script_dir)
  } else if (dir.exists("source 3")) {
    setwd("source 3")
  }
} else {
  if (basename(getwd()) != "source 3" && dir.exists("source 3")) {
    setwd("source 3")
  }
}

set.seed(42)

# --- Pearce-Hall Model (Dynamic Learning Rate) ---

simulate_pearce_hall <- function(design, alpha_base, kappa_init, w0, w1, A, K, sigma_y) {
  n_trials <- nrow(design)
  v_plus <- 0
  v_minus <- 0
  kappa <- kappa_init

  responses <- numeric(n_trials)
  v_plus_trajectory <- numeric(n_trials)
  v_minus_trajectory <- numeric(n_trials)
  kappa_trajectory <- numeric(n_trials)
  alpha_effective_trajectory <- numeric(n_trials)

  for (t in seq_len(n_trials)) {
    g <- design$cs_plus[t] * v_plus + design$cs_minus[t] * v_minus
    theta <- A + (K - A) / (1 + exp(-(w0 + w1 * g)))
    responses[t] <- rnorm(1, theta, sigma_y)

    v_plus_trajectory[t] <- v_plus
    v_minus_trajectory[t] <- v_minus
    kappa_trajectory[t] <- kappa
    alpha_effective_trajectory[t] <- alpha_base * kappa

    if (design$cs_plus[t] == 1) {
      pe <- abs(design$us_plus[t] - v_plus)
    } else {
      pe <- abs(-1 - v_minus)
    }

    kappa <- pe
    alpha_eff <- alpha_base * kappa_trajectory[t]

    if (design$cs_plus[t] == 1) {
      v_plus <- v_plus + alpha_eff * (design$us_plus[t] - v_plus)
    }
    if (design$cs_minus[t] == 1) {
      v_minus <- v_minus + alpha_eff * (-1 - v_minus)
    }
  }

  list(
    responses = responses,
    v_plus = v_plus_trajectory,
    v_minus = v_minus_trajectory,
    kappa = kappa_trajectory,
    alpha_effective = alpha_effective_trajectory
  )
}


# --- Rescorla-Wagner Model (Fixed Learning Rate) ---

simulate_rescorla_wagner <- function(design, alpha, w0, w1, A, K, sigma_y) {
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


# --- Parameter Recovery Functions ---

recover_alpha_pearce_hall <- function(responses, design, A, K, w0, w1, sigma_y, kappa_init = 1.0) {
  if (any(is.na(responses))) {
    return(list(alpha = NA_real_, converged = FALSE))
  }

  log_likelihood_ph <- function(alpha_base) {
    if (alpha_base <= 0.001 || alpha_base >= 0.999) return(-Inf)

    n_trials <- nrow(design)
    v_plus <- 0
    v_minus <- 0
    kappa <- kappa_init
    total_log_lik <- 0

    for (t in seq_len(n_trials)) {
      g <- design$cs_plus[t] * v_plus + design$cs_minus[t] * v_minus
      theta <- A + (K - A) / (1 + exp(-(w0 + w1 * g)))
      total_log_lik <- total_log_lik + dnorm(responses[t], theta, sigma_y, log = TRUE)

      if (design$cs_plus[t] == 1) {
        pe <- abs(design$us_plus[t] - v_plus)
      } else {
        pe <- abs(-1 - v_minus)
      }

      alpha_eff <- alpha_base * kappa

      if (design$cs_plus[t] == 1) {
        v_plus <- v_plus + alpha_eff * (design$us_plus[t] - v_plus)
      }
      if (design$cs_minus[t] == 1) {
        v_minus <- v_minus + alpha_eff * (-1 - v_minus)
      }

      kappa <- pe
    }

    total_log_lik
  }

  opt_result <- tryCatch({
    optimize(
      function(alpha) -log_likelihood_ph(alpha),
      interval = c(0.001, 0.999),
      tol = 1e-6
    )
  }, error = function(e) {
    list(minimum = NA_real_, objective = Inf)
  })

  converged <- is.finite(opt_result$objective)

  list(
    alpha = opt_result$minimum,
    converged = converged
  )
}


recover_alpha_rescorla_wagner <- function(responses, design, A, K, w0, w1, sigma_y) {
  if (any(is.na(responses))) {
    return(list(alpha = NA_real_, converged = FALSE))
  }

  log_likelihood_rw <- function(alpha) {
    if (alpha <= 0.001 || alpha >= 0.999) return(-Inf)

    n_trials <- nrow(design)
    v_plus <- 0
    v_minus <- 0
    total_log_lik <- 0

    for (t in seq_len(n_trials)) {
      g <- design$cs_plus[t] * v_plus + design$cs_minus[t] * v_minus
      theta <- A + (K - A) / (1 + exp(-(w0 + w1 * g)))
      total_log_lik <- total_log_lik + dnorm(responses[t], theta, sigma_y, log = TRUE)

      if (design$cs_plus[t] == 1) {
        v_plus <- v_plus + alpha * (design$us_plus[t] - v_plus)
      }
      if (design$cs_minus[t] == 1) {
        v_minus <- v_minus + alpha * (-1 - v_minus)
      }
    }

    total_log_lik
  }

  opt_result <- tryCatch({
    optimize(
      function(alpha) -log_likelihood_rw(alpha),
      interval = c(0.001, 0.999),
      tol = 1e-6
    )
  }, error = function(e) {
    list(minimum = NA_real_, objective = Inf)
  })

  converged <- is.finite(opt_result$objective)

  list(
    alpha = opt_result$minimum,
    converged = converged
  )
}


# --- Experimental Design Generation ---

create_experimental_design <- function(n_trials = 40,
                                      reinforcement_rate = 0.5,
                                      ordering = "random") {
  n_cs_plus <- n_trials / 2
  n_cs_minus <- n_trials / 2

  if (ordering == "random") {
    cs_sequence <- sample(rep(c(1, 0), c(n_cs_plus, n_cs_minus)))
  } else if (ordering == "alternating") {
    cs_sequence <- rep(c(1, 0), length.out = n_trials)
  }

  n_shocks <- round(n_cs_plus * reinforcement_rate)
  cs_plus_trials <- which(cs_sequence == 1)
  shock_trials <- sample(cs_plus_trials, n_shocks)

  us_sequence <- numeric(n_trials)
  us_sequence[shock_trials] <- 1

  data.frame(
    cs_plus = cs_sequence,
    cs_minus = 1 - cs_sequence,
    us_plus = us_sequence
  )
}


# --- Main Simulation ---

run_model_misspecification_simulation <- function(
    n_participants = 1000,
    n_trials = 40,
    reinforcement_rate = 0.5,
    sigma_y = 5,
    A_param = 0,
    K_param = 100
) {

  alpha_base_true <- runif(n_participants, min = 0.1, max = 0.9)

  w0 <- pmin(pmax(rnorm(n_participants, mean = -1, sd = 1.5), -5), 4)
  w1 <- runif(n_participants, min = 2, max = 8)

  participants <- data.frame(
    participant_id = 1:n_participants,
    alpha_base_true = alpha_base_true,
    w0 = w0,
    w1 = w1
  )

  results <- data.frame(
    participant_id = participants$participant_id,
    alpha_base_true = participants$alpha_base_true,
    alpha_recovered_correct_model = NA_real_,
    alpha_recovered_wrong_model = NA_real_,
    converged_correct = FALSE,
    converged_wrong = FALSE
  )

  for (i in 1:n_participants) {

    design <- create_experimental_design(
      n_trials = n_trials,
      reinforcement_rate = reinforcement_rate,
      ordering = "random"
    )

    sim_data <- simulate_pearce_hall(
      design = design,
      alpha_base = participants$alpha_base_true[i],
      kappa_init = 1.0,
      w0 = participants$w0[i],
      w1 = participants$w1[i],
      A = A_param,
      K = K_param,
      sigma_y = sigma_y
    )

    responses <- sim_data$responses

    recovery_correct <- recover_alpha_pearce_hall(
      responses = responses,
      design = design,
      A = A_param,
      K = K_param,
      w0 = participants$w0[i],
      w1 = participants$w1[i],
      sigma_y = sigma_y,
      kappa_init = 1.0
    )

    recovery_wrong <- recover_alpha_rescorla_wagner(
      responses = responses,
      design = design,
      A = A_param,
      K = K_param,
      w0 = participants$w0[i],
      w1 = participants$w1[i],
      sigma_y = sigma_y
    )

    results$alpha_recovered_correct_model[i] <- recovery_correct$alpha
    results$alpha_recovered_wrong_model[i] <- recovery_wrong$alpha
    results$converged_correct[i] <- recovery_correct$converged
    results$converged_wrong[i] <- recovery_wrong$converged
  }

  valid_correct <- complete.cases(results$alpha_base_true, results$alpha_recovered_correct_model)
  valid_wrong <- complete.cases(results$alpha_base_true, results$alpha_recovered_wrong_model)

  R2_correct <- cor(
    results$alpha_base_true[valid_correct],
    results$alpha_recovered_correct_model[valid_correct]
  )^2

  R2_wrong <- cor(
    results$alpha_base_true[valid_wrong],
    results$alpha_recovered_wrong_model[valid_wrong]
  )^2

  cat("Correct model R2:", round(R2_correct, 3),
      "| Wrong model R2:", round(R2_wrong, 3), "\n")

  save(
    results,
    participants,
    R2_correct,
    R2_wrong,
    file = "model_misspecification_results.RData"
  )

  write.csv(
    results,
    "model_misspecification_summary.csv",
    row.names = FALSE
  )

  return(results)
}


# --- Visualization Function ---

create_model_misspecification_figure <- function(results_path = "model_misspecification_results.RData") {

  load(results_path)

  # Panel B processed first to identify example participants shared across both panels
  panel_b_data <- results %>%
    filter(!is.na(alpha_recovered_wrong_model))

  R2_b <- cor(panel_b_data$alpha_base_true, panel_b_data$alpha_recovered_wrong_model)^2

  # Find participants with very different true alphas but similar wrong-model estimates
  best_pair <- NULL; best_score <- Inf
  for (ii in 1:(nrow(panel_b_data)-1)) {
    for (jj in (ii+1):nrow(panel_b_data)) {
      true_diff <- abs(panel_b_data$alpha_base_true[ii] - panel_b_data$alpha_base_true[jj])
      wrong_diff <- abs(panel_b_data$alpha_recovered_wrong_model[ii] - panel_b_data$alpha_recovered_wrong_model[jj])
      if (true_diff > 0.4 && wrong_diff < 0.05) {
        score <- wrong_diff / true_diff
        if (score < best_score) { best_score <- score; best_pair <- c(ii, jj) }
      }
    }
  }

  if (!is.null(best_pair)) {
    example1 <- panel_b_data[best_pair[1], ]
    example2 <- panel_b_data[best_pair[2], ]

    panel_b_data <- panel_b_data %>%
      mutate(
        is_example = participant_id %in% c(example1$participant_id, example2$participant_id)
      )
  } else {
    panel_b_data$is_example <- FALSE
    example1 <- NULL
    example2 <- NULL
  }

  panel_a_data <- results %>%
    filter(!is.na(alpha_recovered_correct_model))

  R2_a <- cor(panel_a_data$alpha_base_true, panel_a_data$alpha_recovered_correct_model)^2

  if (!is.null(example1) && !is.null(example2)) {
    panel_a_data <- panel_a_data %>%
      mutate(
        is_example = participant_id %in% c(example1$participant_id, example2$participant_id)
      )

    example1_panel_a <- panel_a_data %>%
      filter(participant_id == example1$participant_id)
    example2_panel_a <- panel_a_data %>%
      filter(participant_id == example2$participant_id)
  } else {
    panel_a_data$is_example <- FALSE
    example1_panel_a <- NULL
    example2_panel_a <- NULL
  }

  # --- Panel A plot ---
  p_a <- ggplot(panel_a_data, aes(x = alpha_base_true, y = alpha_recovered_correct_model)) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray40", linewidth = 0.5) +
    geom_point(data = panel_a_data %>% filter(!is_example),
               alpha = 0.25, size = 1.2, color = "#1565C0") +

    {if (!is.null(example1_panel_a) && !is.null(example2_panel_a) &&
         nrow(example1_panel_a) > 0 && nrow(example2_panel_a) > 0) {
      annotate("segment",
              x = example1_panel_a$alpha_base_true,
              y = example1_panel_a$alpha_recovered_correct_model,
              xend = example2_panel_a$alpha_base_true,
              yend = example2_panel_a$alpha_recovered_correct_model,
              color = "black", linewidth = 0.8, linetype = "solid", alpha = 0.6)
    }} +

    geom_point(data = panel_a_data %>% filter(is_example),
               size = 3.5, color = "black", shape = 16, alpha = 1) +

    annotate("text", x = 0.03, y = 0.97,
             label = "Correct model\n(Pearce\u2013Hall)",
             size = 3.2, color = "#1565C0", fontface = "italic",
             hjust = 0, vjust = 1) +

    annotate("text", x = 0.97, y = 0.03,
             label = sprintf("R\u00b2 = %.2f", R2_a),
             size = 4, fontface = "bold", color = "#1976D2",
             hjust = 1, vjust = 0) +

    scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2),
                       labels = sprintf("%.1f", seq(0, 1, 0.2)), expand = c(0.02, 0)) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2),
                       labels = sprintf("%.1f", seq(0, 1, 0.2)), expand = c(0.02, 0)) +

    labs(x = "True learning rate", y = "Recovered learning rate") +

    theme_minimal(base_size = 12, base_family = 'Helvetica') +
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      panel.grid.major = element_line(color = "gray90", linewidth = 0.3),
      panel.grid.minor = element_blank(),
      axis.title = element_text(size = 11, face = "bold", color = "gray20"),
      axis.text = element_text(size = 10, color = "gray30"),
      plot.margin = margin(t = 5, r = 10, b = 5, l = 5),
      plot.tag = element_text(size = 14, face = "bold")
    ) +

    coord_fixed(clip = "off") +
    labs(tag = "a")

  # --- Panel B plot ---
  p_b <- ggplot(panel_b_data, aes(x = alpha_base_true, y = alpha_recovered_wrong_model)) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray40", linewidth = 0.5) +
    geom_point(data = panel_b_data %>% filter(!is_example),
               alpha = 0.25, size = 1.2, color = "#E57373") +

    {if (!is.null(example1) && !is.null(example2)) {
      annotate("segment",
              x = example1$alpha_base_true,
              y = example1$alpha_recovered_wrong_model,
              xend = example2$alpha_base_true,
              yend = example2$alpha_recovered_wrong_model,
              color = "black", linewidth = 0.8, linetype = "solid", alpha = 0.6)
    }} +

    geom_point(data = panel_b_data %>% filter(is_example),
               size = 3.5, color = "black", shape = 16, alpha = 1) +

    annotate("text", x = 0.03, y = 0.97,
             label = "Misspecified model\n(Rescorla\u2013Wagner)",
             size = 3.2, color = "#E64A19", fontface = "italic",
             hjust = 0, vjust = 1) +

    annotate("text", x = 0.97, y = 0.03,
             label = sprintf("R\u00b2 = %.2f", R2_b),
             size = 4, fontface = "bold", color = "#E64A19",
             hjust = 1, vjust = 0) +

    {if (!is.null(example1) && !is.null(example2)) {
      list(
        annotate("segment",
                x = example1$alpha_base_true,
                y = example1$alpha_recovered_wrong_model,
                xend = example1$alpha_base_true + 0.02,
                yend = example1$alpha_recovered_wrong_model + 0.12,
                arrow = arrow(length = unit(0.15, "cm"), type = "closed"),
                color = "black", linewidth = 0.7),
        annotate("text",
                x = example1$alpha_base_true + 0.02,
                y = example1$alpha_recovered_wrong_model + 0.16,
                label = sprintf("True = %.2f", example1$alpha_base_true),
                size = 2.8, fontface = "bold", color = "black", hjust = 0.5),

        annotate("segment",
                x = example2$alpha_base_true,
                y = example2$alpha_recovered_wrong_model,
                xend = example2$alpha_base_true - 0.02,
                yend = example2$alpha_recovered_wrong_model + 0.12,
                arrow = arrow(length = unit(0.15, "cm"), type = "closed"),
                color = "black", linewidth = 0.7),
        annotate("text",
                x = example2$alpha_base_true - 0.02,
                y = example2$alpha_recovered_wrong_model + 0.16,
                label = sprintf("True = %.2f", example2$alpha_base_true),
                size = 2.8, fontface = "bold", color = "black", hjust = 0.5)
      )
    }} +

    scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2),
                       labels = sprintf("%.1f", seq(0, 1, 0.2)), expand = c(0.02, 0)) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2),
                       labels = sprintf("%.1f", seq(0, 1, 0.2)), expand = c(0.02, 0)) +

    labs(x = "True learning rate", y = "Recovered learning rate") +

    theme_minimal(base_size = 12, base_family = 'Helvetica') +
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      panel.grid.major = element_line(color = "gray90", linewidth = 0.3),
      panel.grid.minor = element_blank(),
      axis.title = element_text(size = 11, face = "bold", color = "gray20"),
      axis.text = element_text(size = 10, color = "gray30"),
      plot.margin = margin(t = 5, r = 5, b = 5, l = 10),
      plot.tag = element_text(size = 14, face = "bold")
    ) +

    coord_fixed(clip = "off") +
    labs(tag = "b")


  # Nature double-column width: 180mm = 7.09in
  p_combined <- p_a + p_b +
    plot_layout(ncol = 2, widths = c(1, 1))

  ggsave(
    "fig3_model_misspecification.pdf",
    p_combined,
    width = 7.09,
    height = 3.5,
    bg = "white",
    units = "in"
  )

  ggsave(
    "fig3_model_misspecification.png",
    p_combined,
    width = 7.09,
    height = 3.5,
    dpi = 600,
    bg = "white",
    units = "in"
  )

  return(p_combined)
}


# --- Main Execution ---

main <- function() {
  results <- run_model_misspecification_simulation(
    n_participants = 1000,
    n_trials = 40,
    reinforcement_rate = 0.5,
    sigma_y = 5,
    A_param = 0,
    K_param = 100
  )

  plot <- create_model_misspecification_figure()

  invisible(list(results = results, plot = plot))
}


# --- Run ---

if (!interactive()) {
  results <- main()
}
