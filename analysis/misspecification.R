# Cross-model misspecification: 2x2 recovery simulation + main/supp figures.

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, patchwork)

res_dir <- file.path(dirname(getwd()), "results")
fig_dir <- file.path(dirname(getwd()), "figures")

set.seed(42)

simulate_pearce_hall <- function(design, alpha_base, kappa_init, w0, w1, A, K, sigma_y) {
  n_trials <- nrow(design)
  v_plus <- 0
  v_minus <- 0
  kappa <- kappa_init

  responses <- numeric(n_trials)

  for (t in seq_len(n_trials)) {
    g <- design$cs_plus[t] * v_plus + design$cs_minus[t] * v_minus
    theta <- A + (K - A) / (1 + exp(-(w0 + w1 * g)))
    responses[t] <- rnorm(1, theta, sigma_y)

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

  responses
}


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


recover_alpha_pearce_hall <- function(responses, design, A, K, w0, w1, sigma_y,
                                      kappa_init = 1.0) {
  if (any(is.na(responses))) return(NA_real_)

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
    optimize(function(alpha) -log_likelihood_ph(alpha),
             interval = c(0.001, 0.999), tol = 1e-6)
  }, error = function(e) list(minimum = NA_real_, objective = Inf))

  if (!is.finite(opt_result$objective)) return(NA_real_)
  opt_result$minimum
}


recover_alpha_rescorla_wagner <- function(responses, design, A, K, w0, w1, sigma_y) {
  if (any(is.na(responses))) return(NA_real_)

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
    optimize(function(alpha) -log_likelihood_rw(alpha),
             interval = c(0.001, 0.999), tol = 1e-6)
  }, error = function(e) list(minimum = NA_real_, objective = Inf))

  if (!is.finite(opt_result$objective)) return(NA_real_)
  opt_result$minimum
}


create_experimental_design <- function(n_trials = 40, reinforcement_rate = 0.5) {
  n_cs_plus <- n_trials / 2
  n_cs_minus <- n_trials / 2
  cs_sequence <- sample(rep(c(1, 0), c(n_cs_plus, n_cs_minus)))

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


run_cross_model_recovery <- function(n_participants = 500,
                                     n_trials = 40,
                                     reinforcement_rate = 0.5,
                                     sigma_y = 5,
                                     A_param = 0,
                                     K_param = 100) {

  # Same drawing distributions as model_misspecification_simulation.R
  alpha_true <- runif(n_participants, min = 0.1, max = 0.9)
  w0 <- pmin(pmax(rnorm(n_participants, mean = -1, sd = 1.5), -5), 4)
  w1 <- runif(n_participants, min = 2, max = 8)

  results <- data.frame(
    participant_id = seq_len(n_participants),
    alpha_true = alpha_true,
    w0 = w0,
    w1 = w1,
    ph_data_ph_fit = NA_real_,  # PH data, PH recovery (correct)
    ph_data_rw_fit = NA_real_,  # PH data, RW recovery (wrong)
    rw_data_rw_fit = NA_real_,  # RW data, RW recovery (correct)
    rw_data_ph_fit = NA_real_   # RW data, PH recovery (wrong)
  )

  for (i in seq_len(n_participants)) {

    design <- create_experimental_design(
      n_trials = n_trials,
      reinforcement_rate = reinforcement_rate
    )

    ph_responses <- simulate_pearce_hall(
      design = design,
      alpha_base = alpha_true[i],
      kappa_init = 1.0,
      w0 = w0[i], w1 = w1[i],
      A = A_param, K = K_param,
      sigma_y = sigma_y
    )

    rw_responses <- simulate_rescorla_wagner(
      design = design,
      alpha = alpha_true[i],
      w0 = w0[i], w1 = w1[i],
      A = A_param, K = K_param,
      sigma_y = sigma_y
    )

    results$ph_data_ph_fit[i] <- recover_alpha_pearce_hall(
      responses = ph_responses, design = design,
      A = A_param, K = K_param,
      w0 = w0[i], w1 = w1[i], sigma_y = sigma_y, kappa_init = 1.0
    )

    results$ph_data_rw_fit[i] <- recover_alpha_rescorla_wagner(
      responses = ph_responses, design = design,
      A = A_param, K = K_param,
      w0 = w0[i], w1 = w1[i], sigma_y = sigma_y
    )

    results$rw_data_rw_fit[i] <- recover_alpha_rescorla_wagner(
      responses = rw_responses, design = design,
      A = A_param, K = K_param,
      w0 = w0[i], w1 = w1[i], sigma_y = sigma_y
    )

    results$rw_data_ph_fit[i] <- recover_alpha_pearce_hall(
      responses = rw_responses, design = design,
      A = A_param, K = K_param,
      w0 = w0[i], w1 = w1[i], sigma_y = sigma_y, kappa_init = 1.0
    )
  }

  r2 <- function(x, y) {
    ok <- complete.cases(x, y)
    cor(x[ok], y[ok])^2
  }

  R2_matrix <- matrix(
    c(r2(results$alpha_true, results$ph_data_ph_fit),
      r2(results$alpha_true, results$ph_data_rw_fit),
      r2(results$alpha_true, results$rw_data_ph_fit),
      r2(results$alpha_true, results$rw_data_rw_fit)),
    nrow = 2, byrow = TRUE,
    dimnames = list(
      `Data generating model` = c("Pearce-Hall", "Rescorla-Wagner"),
      `Recovery model`        = c("Pearce-Hall", "Rescorla-Wagner")
    )
  )

  cat("\nCross-model recovery: R^2(true, recovered)\n")
  print(round(R2_matrix, 3))
  cat("Diagonal = correct specification; off-diagonal = misspecified.\n\n")

  save(results, R2_matrix,
       file = "../results/cross_model_recovery_results.RData")

  write.csv(results, "../results/cross_model_recovery_summary.csv", row.names = FALSE)

  invisible(list(results = results, R2_matrix = R2_matrix))
}


cache <- file.path(res_dir, "cross_model_recovery_results.RData")
if (!file.exists(cache)) {
  run_cross_model_recovery(
    n_participants = 500,
    n_trials = 40,
    reinforcement_rate = 0.5,
    sigma_y = 5,
    A_param = 0,
    K_param = 100
  )
}


# Panel a: 2x2 cross-model grid

load(file.path(res_dir, "cross_model_recovery_results.RData"))

cross_long <- results %>%
  pivot_longer(
    cols = c(ph_data_ph_fit, ph_data_rw_fit, rw_data_rw_fit, rw_data_ph_fit),
    names_to = "condition",
    values_to = "alpha_recovered"
  ) %>%
  mutate(
    gen_model = ifelse(grepl("^ph_data", condition), "PH-generated data", "RW-generated data"),
    fit_model = ifelse(grepl("ph_fit$", condition), "PH recovery", "RW recovery"),
    correct = (gen_model == "PH-generated data" & fit_model == "PH recovery") |
              (gen_model == "RW-generated data" & fit_model == "RW recovery")
  )

r2_labels <- cross_long %>%
  group_by(gen_model, fit_model, correct) %>%
  summarize(
    r2 = cor(alpha_true, alpha_recovered, use = "complete.obs")^2,
    .groups = "drop"
  ) %>%
  mutate(label = sprintf("R² = %.2f", r2))

find_collapse_pair <- function(data, true_col, recovered_col,
                                min_true_diff = 0.4, max_recov_diff = 0.05) {
  best_pair <- NULL
  best_score <- Inf
  for (ii in 1:(nrow(data) - 1)) {
    for (jj in (ii + 1):nrow(data)) {
      true_diff <- abs(data[[true_col]][ii] - data[[true_col]][jj])
      recov_diff <- abs(data[[recovered_col]][ii] - data[[recovered_col]][jj])
      if (true_diff > min_true_diff && recov_diff < max_recov_diff) {
        score <- recov_diff / true_diff
        if (score < best_score) {
          best_score <- score
          best_pair <- c(ii, jj)
        }
      }
    }
  }
  # Ensure lower alpha is first
  if (!is.null(best_pair)) {
    if (data[[true_col]][best_pair[1]] > data[[true_col]][best_pair[2]]) {
      best_pair <- rev(best_pair)
    }
  }
  best_pair
}

misspec1 <- results %>% filter(!is.na(ph_data_rw_fit))
pair1_idx <- find_collapse_pair(misspec1, "alpha_true", "ph_data_rw_fit")
ex1_lo <- misspec1[pair1_idx[1], ]
ex1_hi <- misspec1[pair1_idx[2], ]
cat(sprintf("  PH→RW collapse: ID %d (true=%.2f, recov=%.2f), ID %d (true=%.2f, recov=%.2f)\n",
            ex1_lo$participant_id, ex1_lo$alpha_true, ex1_lo$ph_data_rw_fit,
            ex1_hi$participant_id, ex1_hi$alpha_true, ex1_hi$ph_data_rw_fit))

misspec2 <- results %>% filter(!is.na(rw_data_ph_fit))
pair2_idx <- find_collapse_pair(misspec2, "alpha_true", "rw_data_ph_fit")
ex2_lo <- misspec2[pair2_idx[1], ]
ex2_hi <- misspec2[pair2_idx[2], ]
cat(sprintf("  RW→PH collapse: ID %d (true=%.2f, recov=%.2f), ID %d (true=%.2f, recov=%.2f)\n",
            ex2_lo$participant_id, ex2_lo$alpha_true, ex2_lo$rw_data_ph_fit,
            ex2_hi$participant_id, ex2_hi$alpha_true, ex2_hi$rw_data_ph_fit))

col_lo <- "#E69F00"   # orange - lower true alpha
col_hi <- "#009E73"   # green - higher true alpha

level_gen <- c("PH-generated data", "RW-generated data")
level_fit <- c("PH recovery", "RW recovery")

hl_points_top <- data.frame(
  gen_model = factor("PH-generated data", levels = level_gen),
  fit_model = factor(rep(c("PH recovery", "RW recovery"), each = 2), levels = level_fit),
  alpha_true = c(ex1_lo$alpha_true, ex1_hi$alpha_true,
                 ex1_lo$alpha_true, ex1_hi$alpha_true),
  alpha_recovered = c(ex1_lo$ph_data_ph_fit, ex1_hi$ph_data_ph_fit,
                      ex1_lo$ph_data_rw_fit, ex1_hi$ph_data_rw_fit),
  fill_col = rep(c(col_lo, col_hi), 2)
)

hl_points_bot <- data.frame(
  gen_model = factor("RW-generated data", levels = level_gen),
  fit_model = factor(rep(c("PH recovery", "RW recovery"), each = 2), levels = level_fit),
  alpha_true = c(ex2_lo$alpha_true, ex2_hi$alpha_true,
                 ex2_lo$alpha_true, ex2_hi$alpha_true),
  alpha_recovered = c(ex2_lo$rw_data_ph_fit, ex2_hi$rw_data_ph_fit,
                      ex2_lo$rw_data_rw_fit, ex2_hi$rw_data_rw_fit),
  fill_col = rep(c(col_lo, col_hi), 2)
)

hl_points <- bind_rows(hl_points_top, hl_points_bot)

seg_data <- data.frame(
  gen_model = factor(c("PH-generated data", "PH-generated data",
                       "RW-generated data", "RW-generated data"), levels = level_gen),
  fit_model = factor(c("PH recovery", "RW recovery",
                       "PH recovery", "RW recovery"), levels = level_fit),
  x    = c(ex1_lo$alpha_true, ex1_lo$alpha_true,
           ex2_lo$alpha_true, ex2_lo$alpha_true),
  xend = c(ex1_hi$alpha_true, ex1_hi$alpha_true,
           ex2_hi$alpha_true, ex2_hi$alpha_true),
  y    = c(ex1_lo$ph_data_ph_fit, ex1_lo$ph_data_rw_fit,
           ex2_lo$rw_data_ph_fit, ex2_lo$rw_data_rw_fit),
  yend = c(ex1_hi$ph_data_ph_fit, ex1_hi$ph_data_rw_fit,
           ex2_hi$rw_data_ph_fit, ex2_hi$rw_data_rw_fit)
)

# "True = 0.XX" labels in both off-diagonal panels
label_top <- data.frame(
  gen_model = factor("PH-generated data", levels = level_gen),
  fit_model = factor("RW recovery", levels = level_fit),
  x = c(ex1_lo$alpha_true, ex1_hi$alpha_true),
  y = c(ex1_lo$ph_data_rw_fit, ex1_hi$ph_data_rw_fit),
  label = sprintf("True = %.2f", c(ex1_lo$alpha_true, ex1_hi$alpha_true)),
  nudge_x = c(-0.02, 0.02),
  nudge_y = c(0.13, 0.13),
  col = c(col_lo, col_hi)
)
label_bot <- data.frame(
  gen_model = factor("RW-generated data", levels = level_gen),
  fit_model = factor("PH recovery", levels = level_fit),
  x = c(ex2_lo$alpha_true, ex2_hi$alpha_true),
  y = c(ex2_lo$rw_data_ph_fit, ex2_hi$rw_data_ph_fit),
  label = sprintf("True = %.2f", c(ex2_lo$alpha_true, ex2_hi$alpha_true)),
  nudge_x = c(-0.02, 0.02),
  nudge_y = c(0.13, 0.13),
  col = c(col_lo, col_hi)
)
label_data <- bind_rows(label_top, label_bot)

all_highlight_ids <- c(ex1_lo$participant_id, ex1_hi$participant_id,
                       ex2_lo$participant_id, ex2_hi$participant_id)
bg_data <- cross_long %>% filter(!participant_id %in% all_highlight_ids)

bg_data$gen_model <- factor(bg_data$gen_model, levels = level_gen)
bg_data$fit_model <- factor(bg_data$fit_model, levels = level_fit)
r2_labels$gen_model <- factor(r2_labels$gen_model, levels = level_gen)
r2_labels$fit_model <- factor(r2_labels$fit_model, levels = level_fit)

panel_a <- ggplot() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed",
              colour = "grey60", linewidth = 0.4) +
  geom_point(data = bg_data, aes(x = alpha_true, y = alpha_recovered, colour = correct),
             size = 0.6, alpha = 0.3, stroke = 0) +
  geom_segment(data = seg_data,
               aes(x = x, y = y, xend = xend, yend = yend),
               colour = "grey30", linewidth = 0.7, alpha = 0.6) +
  geom_point(data = hl_points,
             aes(x = alpha_true, y = alpha_recovered),
             fill = hl_points$fill_col, colour = "black",
             size = 3, shape = 21, stroke = 0.7) +
  geom_segment(data = label_data,
               aes(x = x, y = y,
                   xend = x + nudge_x, yend = y + nudge_y),
               arrow = arrow(length = unit(0.12, "cm"), type = "closed"),
               colour = label_data$col, linewidth = 0.6) +
  geom_text(data = label_data,
            aes(x = x + nudge_x, y = y + nudge_y + 0.04, label = label),
            size = 2.6, fontface = "bold", colour = label_data$col) +
  geom_label(data = r2_labels, aes(x = 0.15, y = 0.92, label = label),
             size = 3.2, fontface = "bold",
             label.size = 0, fill = "white", alpha = 0.8) +
  facet_grid(gen_model ~ fit_model) +
  scale_colour_manual(values = c("TRUE" = "#0072B2", "FALSE" = "#D55E00"), guide = "none") +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
  labs(x = "True learning rate",
       y = "Recovered learning rate",
       tag = "a") +
  coord_equal() +
  theme_minimal(base_size = 11) +
  theme(
    plot.tag = element_text(face = "bold", size = 14),
    strip.text = element_text(face = "bold", size = 10),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(0.6, "lines"),
    axis.title = element_text(size = 10),
    plot.margin = margin(t = 5, r = 5, b = 5, l = 5)
  )



ggsave(file.path(fig_dir, "fig5_misspecification_crossonly.pdf"),
       panel_a + theme(plot.tag = element_blank()),
       width = 6.5, height = 6.2, device = cairo_pdf)
