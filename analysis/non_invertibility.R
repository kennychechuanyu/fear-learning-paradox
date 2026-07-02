# Fig 2: non-invertibility existence proof. Four (alpha, schedule) pairs whose three behavioral summaries nearly coincide while RW learning rates are recoverable.

suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(patchwork)
})

set.seed(20260414)

fig_dir <- file.path(dirname(getwd()), "figures")

# fixed response-function parameters
A          <- 0
K          <- 100
w0_fixed   <- -1
w1_fixed   <- 4
n_trials   <- 20
n_cs_plus  <- 10

TRUE_ALPHAS <- c(0.10, 0.25, 0.40, 0.55)

# Okabe-Ito palette
PALETTE <- c("#0072B2", "#E69F00", "#009E73", "#CC79A7")

# noiseless RW + sigmoid simulator
simulate_trajectory <- function(design, alpha, w0 = w0_fixed, w1 = w1_fixed,
                                A_ = A, K_ = K) {
  n <- nrow(design)
  v_plus  <- 0
  v_minus <- 0
  resp <- numeric(n)
  for (t in seq_len(n)) {
    g <- design$cs_plus[t] * v_plus + design$cs_minus[t] * v_minus
    resp[t] <- A_ + (K_ - A_) / (1 + exp(-(w0 + w1 * g)))
    if (design$cs_plus[t] == 1) {
      v_plus  <- v_plus  + alpha * (design$us_plus[t] - v_plus)
    }
    if (design$cs_minus[t] == 1) {
      v_minus <- v_minus + alpha * (-1 - v_minus)
    }
  }
  list(responses = resp)
}

# behavioral summaries (CS+ only)
behavioral_metrics <- function(responses, cs_plus_indicator) {
  csp <- responses[cs_plus_indicator == 1]
  nc <- length(csp)
  slope <- unname(coef(lm(csp ~ seq_len(nc)))[2])
  list(
    slope      = slope,
    first_last = csp[nc] - csp[1],
    last_trial = csp[nc]
  )
}

# fixed CS+/CS- alternation; only the reinforcement pattern varies across participants
cs_plus_positions <- seq(1, n_trials, by = 2)

make_design <- function(reinforced_cs_plus_idx) {
  # reinforced_cs_plus_idx: logical, which CS+ trials are reinforced
  cs_plus  <- as.numeric(seq_len(n_trials) %in% cs_plus_positions)
  cs_minus <- 1 - cs_plus
  us_plus  <- numeric(n_trials)
  us_plus[cs_plus_positions[reinforced_cs_plus_idx]] <- 1
  data.frame(cs_plus = cs_plus, cs_minus = cs_minus, us_plus = us_plus)
}

# enumerate all 2^10 = 1024 schedules per alpha
all_schedules <- function() {
  grid <- expand.grid(replicate(n_cs_plus, c(FALSE, TRUE), simplify = FALSE))
  as.matrix(grid)
}

SCHEDULES <- all_schedules()
cat("Enumerating", nrow(SCHEDULES), "reinforcement schedules per alpha.\n")

# for each alpha, summary for every schedule
summaries_for_alpha <- function(alpha) {
  out <- matrix(NA_real_, nrow = nrow(SCHEDULES), ncol = 3)
  colnames(out) <- c("slope", "first_last", "last_trial")
  for (i in seq_len(nrow(SCHEDULES))) {
    design <- make_design(SCHEDULES[i, ])
    sim <- simulate_trajectory(design, alpha)
    m <- behavioral_metrics(sim$responses, design$cs_plus)
    out[i, ] <- c(m$slope, m$first_last, m$last_trial)
  }
  out
}

SUMMARY_TABLES <- lapply(TRUE_ALPHAS, summaries_for_alpha)
names(SUMMARY_TABLES) <- as.character(TRUE_ALPHAS)

# two-stage constrained search: seed over each alpha's schedules, find nearest neighbour in each other alpha's table, score by max per-metric spread

# constrain so every participant visibly learns (avoid the zero-reinforcement fixed point)
N_REINF_FIXED <- 7   # 70% reinforcement for all
LAST_MIN  <- 45      # asymptote must clear the w0=-1 baseline (~27)

valid_idx_per_alpha <- lapply(seq_along(TRUE_ALPHAS), function(a_idx) {
  n_reinf <- rowSums(SCHEDULES)
  last_trial <- SUMMARY_TABLES[[a_idx]][, "last_trial"]
  which(n_reinf == N_REINF_FIXED & last_trial >= LAST_MIN)
})
cat("Valid schedules per alpha (after constraints):",
    paste(sapply(valid_idx_per_alpha, length), collapse = ", "), "\n")

score_target_constrained <- function(target, tables, valid_lists) {
  nearest <- list()
  for (a_idx in seq_along(tables)) {
    tbl <- tables[[a_idx]]
    valid <- valid_lists[[a_idx]]
    scale <- pmax(abs(target), 1e-6)
    diffs <- sweep(tbl[valid, , drop = FALSE], 2, target, FUN = "-")
    d <- rowSums(diffs^2 / matrix(scale^2, length(valid), 3, byrow = TRUE))
    k <- which.min(d)
    row_idx <- valid[k]
    nearest[[a_idx]] <- list(row = row_idx, summary = tbl[row_idx, ])
  }
  summaries <- do.call(rbind, lapply(nearest, `[[`, "summary"))
  means <- colMeans(summaries)
  spreads <- apply(summaries, 2, function(x) (max(x) - min(x)) / max(abs(means), 1e-6))
  list(nearest = nearest, summaries = summaries,
       score = max(abs(spreads)), spreads = spreads, means = means)
}

# Seed over all valid schedules of each alpha.
best <- list(score = Inf)
for (a_idx in seq_along(TRUE_ALPHAS)) {
  tbl <- SUMMARY_TABLES[[a_idx]]
  for (i in valid_idx_per_alpha[[a_idx]]) {
    res <- score_target_constrained(tbl[i, ], SUMMARY_TABLES, valid_idx_per_alpha)
    if (res$score < best$score) {
      best <- res
    }
  }
}

cat("Best max-spread-fraction across three metrics:",
    sprintf("%.4f", best$score), "\n")

# assemble chosen participants
participants <- lapply(seq_along(TRUE_ALPHAS), function(i) {
  row_idx <- best$nearest[[i]]$row
  schedule <- SCHEDULES[row_idx, ]
  design <- make_design(schedule)
  sim <- simulate_trajectory(design, TRUE_ALPHAS[i])
  m <- behavioral_metrics(sim$responses, design$cs_plus)
  list(
    id = paste0("P", i),
    true_alpha = TRUE_ALPHAS[i],
    schedule = schedule,
    n_reinforced = sum(schedule),
    design = design,
    responses = sim$responses,
    slope = m$slope,
    first_last = m$first_last,
    last_trial = m$last_trial
  )
})

# joint MLE over (alpha, w0, w1), no oracle nuisance parameters
recover_joint <- function(noisy_responses, design, A_ = A, K_ = K) {
  neg_ss <- function(par) {
    a <- par[1]; w0 <- par[2]; w1 <- par[3]
    n <- nrow(design); v_plus <- 0; v_minus <- 0; ss <- 0
    for (t in seq_len(n)) {
      g <- design$cs_plus[t] * v_plus + design$cs_minus[t] * v_minus
      theta <- A_ + (K_ - A_) / (1 + exp(-(w0 + w1 * g)))
      ss <- ss + (noisy_responses[t] - theta)^2
      if (design$cs_plus[t] == 1)
        v_plus <- v_plus + a * (design$us_plus[t] - v_plus)
      if (design$cs_minus[t] == 1)
        v_minus <- v_minus + a * (-1 - v_minus)
    }
    ss
  }
  starts <- list(c(0.3, -2, 3), c(0.5, -1, 4), c(0.7, 0, 5))
  best_val <- Inf; best_par <- starts[[1]]
  for (s in starts) {
    tryCatch({
      opt <- optim(s, neg_ss, method = "L-BFGS-B",
                   lower = c(0.01, -5, 0.5),
                   upper = c(0.99,  4, 15),
                   control = list(maxit = 500))
      if (opt$value < best_val) { best_val <- opt$value; best_par <- opt$par }
    }, error = function(e) NULL)
  }
  c(alpha = best_par[1], w0 = best_par[2], w1 = best_par[3])
}

# panel C: 30 noisy realizations per participant, recovered jointly
N_RESAMPLE <- 30
SIGMA_Y <- 5

recovery_list <- list()
for (i in seq_along(participants)) {
  p <- participants[[i]]
  for (r in seq_len(N_RESAMPLE)) {
    noisy_resp <- p$responses + rnorm(n_trials, 0, SIGMA_Y)
    rec <- recover_joint(noisy_resp, p$design)
    recovery_list <- c(recovery_list, list(data.frame(
      participant = p$id,
      true_alpha  = p$true_alpha,
      resample    = r,
      recovered_alpha = rec["alpha"]
    )))
  }
}
recovery_df <- do.call(rbind, recovery_list)

# median recovered alpha per participant
for (i in seq_along(participants)) {
  sub <- recovery_df[recovery_df$participant == participants[[i]]$id, ]
  participants[[i]]$recovered_alpha <- median(sub$recovered_alpha)
}

summary_df <- data.frame(
  participant     = sapply(participants, `[[`, "id"),
  true_alpha      = sapply(participants, `[[`, "true_alpha"),
  recovered_alpha = sapply(participants, `[[`, "recovered_alpha"),
  n_reinforced    = sapply(participants, `[[`, "n_reinforced"),
  slope           = sapply(participants, `[[`, "slope"),
  first_last      = sapply(participants, `[[`, "first_last"),
  last_trial      = sapply(participants, `[[`, "last_trial")
)
cat("\n=== Four participants ===\n")
print(summary_df, row.names = FALSE, digits = 4)

metric_spread <- function(x) (max(x) - min(x)) / mean(x) * 100
cat("\nPercent spread (max-min over mean) across participants:\n")
cat("  slope:      ", sprintf("%.2f%%", metric_spread(summary_df$slope)), "\n")
cat("  first_last: ", sprintf("%.2f%%", metric_spread(summary_df$first_last)), "\n")
cat("  last_trial: ", sprintf("%.2f%%", metric_spread(summary_df$last_trial)), "\n")

# absolute spread in response-scale points
abs_spreads <- c(
  slope      = diff(range(summary_df$slope)),
  first_last = diff(range(summary_df$first_last)),
  last_trial = diff(range(summary_df$last_trial))
)
cat("\nAbsolute spread (response-scale points):\n")
cat("  slope:      ", sprintf("%.2f", abs_spreads["slope"]), "\n")
cat("  first_last: ", sprintf("%.2f", abs_spreads["first_last"]), "\n")
cat("  last_trial: ", sprintf("%.2f", abs_spreads["last_trial"]), "\n")
cat("  maximum:    ", sprintf("%.2f", max(abs_spreads)), "\n")
stopifnot(max(abs_spreads) < 1.0)  # all three summaries stay within 1 response point

# plotting data
traj_df <- do.call(rbind, lapply(participants, function(p) {
  data.frame(
    participant = p$id,
    true_alpha  = p$true_alpha,
    label       = sprintf("P%s  (\u03B1 = %.2f)",
                          sub("P", "", p$id), p$true_alpha),
    trial       = seq_len(n_trials),
    response    = p$responses,
    cs_plus     = p$design$cs_plus == 1,
    reinforced  = p$design$us_plus == 1
  )
}))

traj_df$label <- factor(traj_df$label, levels = unique(traj_df$label))

# CS+ only
traj_cs_plus <- traj_df[traj_df$cs_plus, ]
traj_cs_plus$cs_plus_trial <- ave(traj_cs_plus$trial, traj_cs_plus$participant,
                                  FUN = seq_along)

metrics_long <- summary_df |>
  select(participant, true_alpha, slope, first_last, last_trial) |>
  pivot_longer(cols = c(slope, first_last, last_trial),
               names_to = "metric", values_to = "value") |>
  mutate(
    metric = recode(metric,
                    slope = "Slope",
                    first_last = "First-to-last change",
                    last_trial = "Final response"),
    metric = factor(metric, levels = c("Slope", "First-to-last change",
                                        "Final response")),
    label = sprintf("P%s  (\u03B1 = %.2f)", sub("P", "", participant), true_alpha),
    label = factor(label, levels = levels(traj_df$label))
  )

recov_df <- summary_df |>
  mutate(
    label = sprintf("P%s  (\u03B1 = %.2f)", sub("P", "", participant), true_alpha),
    label = factor(label, levels = levels(traj_df$label))
  )

theme_fig <- function(base_size = 10) {
  theme_classic(base_size = base_size) +
    theme(
      plot.title        = element_text(face = "plain", size = base_size),
      plot.tag          = element_text(face = "bold", size = base_size + 3),
      plot.tag.position = c(0.02, 0.98),
      axis.text         = element_text(color = "black"),
      axis.line         = element_line(linewidth = 0.4),
      axis.ticks        = element_line(linewidth = 0.4),
      legend.position   = "bottom",
      legend.title      = element_blank(),
      legend.key.height = unit(0.4, "lines"),
      legend.margin     = margin(0, 0, 0, 0),
      panel.grid        = element_blank(),
      strip.background  = element_blank(),
      strip.text        = element_text(size = base_size)
    )
}

# panel A: CS+ learning trajectories
reinforced_points <- traj_cs_plus[traj_cs_plus$reinforced, ]

pA <- ggplot(traj_cs_plus, aes(x = cs_plus_trial, y = response,
                               color = label, group = label)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.6) +
  geom_point(data = reinforced_points,
             aes(x = cs_plus_trial, y = response, fill = label),
             shape = 21, color = "black", size = 2.4, stroke = 0.4,
             show.legend = FALSE) +
  scale_color_manual(values = PALETTE) +
  scale_fill_manual(values = PALETTE) +
  scale_x_continuous(breaks = 1:n_cs_plus) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 25)) +
  labs(x = "CS+ trial", y = "Response (0-100)", tag = "a") +
  theme_fig() +
  theme(legend.position = c(0.02, 0.98),
        legend.justification = c(0, 1),
        legend.background = element_rect(fill = alpha("white", 0.8),
                                         color = NA))

# panel B: behavioral summaries
pB <- ggplot(metrics_long, aes(x = metric, y = value, fill = label)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.72,
           color = "black", linewidth = 0.25) +
  scale_fill_manual(values = PALETTE) +
  labs(x = NULL, y = "Summary value", tag = "b") +
  theme_fig() +
  theme(legend.position = "none")

# panel C: recovered vs true alpha
recov_plot_df <- recovery_df |>
  mutate(
    label = sprintf("P%s  (\u03B1 = %.2f)", sub("P", "", participant), true_alpha),
    label = factor(label, levels = levels(traj_df$label))
  )

pC <- ggplot(recov_plot_df, aes(x = label, y = recovered_alpha)) +
  geom_jitter(aes(color = label), width = 0.15, size = 1.2, alpha = 0.5,
              show.legend = FALSE) +
  geom_segment(data = recov_df,
               aes(x = as.numeric(label) - 0.3,
                   xend = as.numeric(label) + 0.3,
                   y = true_alpha, yend = true_alpha),
               linetype = "dashed", color = "black", linewidth = 0.6) +
  stat_summary(aes(color = label), fun = median, geom = "point",
               shape = 18, size = 3.5, show.legend = FALSE) +
  scale_color_manual(values = PALETTE) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  labs(x = NULL, y = "Recovered learning rate",
       tag = "c") +
  theme_fig() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 25, hjust = 1))

fig <- pA + pB + pC + plot_layout(widths = c(1.15, 1.25, 1.0))

out_path <- file.path(fig_dir, "fig2_nonInvertibility.pdf")

ggsave(out_path, fig, width = 13, height = 4.2, device = cairo_pdf)

cat("\nSaved figure to:\n  ", out_path, "\n", sep = "")
