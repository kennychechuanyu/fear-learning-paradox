# Variance decomposition of behavioral summaries (Fig 3d, Fig 4). Two cached sims: rate-only slope decomposition + trial-count reliability, and the eight-summary all-vary decomposition.

suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(patchwork)
  library(psych)
})

res_dir <- file.path(dirname(getwd()), "results")
fig_dir <- file.path(dirname(getwd()), "figures")

# core simulator (shared by both decompositions)

simulate_session <- function(design, alpha, w0, w1, A = 0, K = 100,
                             sigma_y = 5, noise_draws = NULL) {
  n_trials <- nrow(design)
  v_plus <- 0; v_minus <- 0
  responses <- numeric(n_trials)
  for (t in seq_len(n_trials)) {
    g <- design$cs_plus[t] * v_plus + design$cs_minus[t] * v_minus
    theta <- A + (K - A) / (1 + exp(-(w0 + w1 * g)))
    responses[t] <- theta + (if (is.null(noise_draws)) rnorm(1, 0, sigma_y)
                             else noise_draws[t])
    if (design$cs_plus[t] == 1)
      v_plus <- v_plus + alpha * (design$us_plus[t] - v_plus)
    if (design$cs_minus[t] == 1)
      v_minus <- v_minus + alpha * (-1 - v_minus)
  }
  responses
}

make_random_design <- function(n_trials, reinforcement = 0.8) {
  cs_sequence <- sample(rep(c(1, 0), each = n_trials / 2))
  cs_plus_idx <- which(cs_sequence == 1)
  n_shocks <- round(length(cs_plus_idx) * reinforcement)
  shock_positions <- sample(cs_plus_idx, n_shocks)
  data.frame(
    cs_plus  = cs_sequence,
    cs_minus = 1 - cs_sequence,
    us_plus  = as.numeric(seq_len(n_trials) %in% shock_positions)
  )
}

slope_summary <- function(responses, design) {
  csp <- responses[design$cs_plus == 1]
  unname(coef(lm(csp ~ seq_along(csp)))[2])
}

# grid-search alpha recovery with oracle w0, w1
fit_alpha_grid <- function(responses, design, w0, w1, A = 0, K = 100) {
  alphas <- seq(0.01, 0.99, by = 0.01)
  rss <- vapply(alphas, function(a) {
    v_plus <- 0; v_minus <- 0
    ss <- 0
    for (t in seq_along(responses)) {
      g <- design$cs_plus[t] * v_plus + design$cs_minus[t] * v_minus
      theta <- A + (K - A) / (1 + exp(-(w0 + w1 * g)))
      ss <- ss + (responses[t] - theta)^2
      if (design$cs_plus[t] == 1)
        v_plus <- v_plus + a * (design$us_plus[t] - v_plus)
      if (design$cs_minus[t] == 1)
        v_minus <- v_minus + a * (-1 - v_minus)
    }
    ss
  }, numeric(1))
  alphas[which.min(rss)]
}

compute_icc21 <- function(x1, x2) {
  dd <- data.frame(T1 = x1, T2 = x2)
  dd <- dd[complete.cases(dd), ]
  if (nrow(dd) < 10) return(NA)
  tryCatch(ICC(dd, missing = FALSE)$results$ICC[2], error = function(e) NA)
}

# ---- rate-only slope decomposition + trial-count reliability (slow) ----

vd_cache <- file.path(res_dir, "variance_decomposition_improved_results.rds")
if (!file.exists(vd_cache)) {
  set.seed(20260417)

  n_part <- 200
  n_iter_vd  <- 50   # for variance decomposition
  n_iter_icc <- 30   # for ICC computation
  n_anchor   <- 20
  trial_counts <- c(20, 40, 60, 80, 100, 200, 500, 1000)

  # rate-only population: w0/w1 fixed so only the learning rate varies
  participants <- data.frame(
    id    = seq_len(n_part),
    alpha = runif(n_part, 0.05, 0.95),
    w0    = -1,
    w1    = 4
  )

  # spread anchor participants across the alpha range
  anchor_ids <- order(participants$alpha)[
    round(seq(1, n_part, length.out = n_anchor + 2)[-c(1, n_anchor + 2)])
  ]

  decompose_one <- function(n_trials) {
    # (1) Trait: vary participants, fix sequence + noise (averaged over designs)
    n_design_reps <- 10
    var_trait_vec <- vapply(seq_len(n_design_reps), function(dr) {
      fixed_design <- make_random_design(n_trials)
      fixed_noise  <- rnorm(n_trials, 0, 5)
      slopes_trait <- vapply(seq_len(n_part), function(i) {
        p <- participants[i, ]
        r <- simulate_session(fixed_design, p$alpha, p$w0, p$w1,
                              noise_draws = fixed_noise)
        slope_summary(r, fixed_design)
      }, numeric(1))
      var(slopes_trait)
    }, numeric(1))
    var_trait <- mean(var_trait_vec)

    # (2) Sequence: average over anchors, fix noise, vary sequence
    var_seq_vec <- vapply(anchor_ids, function(aid) {
      p <- participants[aid, ]
      fn <- rnorm(n_trials, 0, 5)
      slopes <- vapply(seq_len(n_iter_vd), function(i) {
        d <- make_random_design(n_trials)
        r <- simulate_session(d, p$alpha, p$w0, p$w1, noise_draws = fn)
        slope_summary(r, d)
      }, numeric(1))
      var(slopes)
    }, numeric(1))
    var_sequence <- mean(var_seq_vec)

    # (3) Noise: average over anchors, fix sequence, vary noise
    var_noise_vec <- vapply(anchor_ids, function(aid) {
      p <- participants[aid, ]
      fd <- make_random_design(n_trials)
      slopes <- vapply(seq_len(n_iter_vd), function(i) {
        r <- simulate_session(fd, p$alpha, p$w0, p$w1)
        slope_summary(r, fd)
      }, numeric(1))
      var(slopes)
    }, numeric(1))
    var_noise <- mean(var_noise_vec)

    data.frame(n_trials = n_trials,
               var_trait = var_trait,
               var_sequence = var_sequence,
               var_noise = var_noise)
  }

  vd <- do.call(rbind, lapply(trial_counts, decompose_one))

  # additivity check: actual total between-session variance vs sum of marginals
  cat("Additivity check (sum of marginals / actual total variance):\n")
  for (r in seq_len(nrow(vd))) {
    nt <- vd$n_trials[r]
    slopes_full <- numeric(n_part * 10)
    for (rep in seq_len(10)) {
      for (i in seq_len(n_part)) {
        p <- participants[i, ]
        d <- make_random_design(nt)
        resp <- simulate_session(d, p$alpha, p$w0, p$w1)
        slopes_full[(rep - 1) * n_part + i] <- slope_summary(resp, d)
      }
    }
    var_actual <- var(slopes_full)
    var_sum <- vd$var_trait[r] + vd$var_sequence[r] + vd$var_noise[r]
    ratio <- var_sum / var_actual
    cat(sprintf("  %4d trials: sum=%.4f, actual=%.4f, ratio=%.3f\n",
                nt, var_sum, var_actual, ratio))
    vd$var_actual[r] <- var_actual
    vd$additivity_ratio[r] <- ratio
  }

  vd$var_total <- with(vd, var_trait + var_sequence + var_noise)
  vd$prop_trait    <- vd$var_trait / vd$var_total
  vd$prop_sequence <- vd$var_sequence / vd$var_total
  vd$prop_noise    <- vd$var_noise / vd$var_total

  csplus_summaries <- function(responses, design) {
    csp <- responses[design$cs_plus == 1]
    nc <- length(csp)
    c(slope = unname(coef(lm(csp ~ seq_len(nc)))[2]),
      fl = csp[nc] - csp[1],
      last = csp[nc])
  }

  icc_list <- lapply(trial_counts, function(nt) {
    icc_slope <- icc_fl <- icc_last <- icc_alpha <- numeric(n_iter_icc)

    for (iter in seq_len(n_iter_icc)) {
      # fresh rate-only participants each iteration
      iter_part <- data.frame(
        alpha = runif(n_part, 0.05, 0.95),
        w0    = -1,
        w1    = 4
      )
      sl1 <- sl2 <- fl1 <- fl2 <- la1 <- la2 <- al1 <- al2 <- numeric(n_part)
      for (i in seq_len(n_part)) {
        p <- iter_part[i, ]
        d1 <- make_random_design(nt)
        d2 <- make_random_design(nt)
        r1 <- simulate_session(d1, p$alpha, p$w0, p$w1)
        r2 <- simulate_session(d2, p$alpha, p$w0, p$w1)
        s1 <- csplus_summaries(r1, d1); s2 <- csplus_summaries(r2, d2)
        sl1[i] <- s1["slope"]; sl2[i] <- s2["slope"]
        fl1[i] <- s1["fl"];    fl2[i] <- s2["fl"]
        la1[i] <- s1["last"];  la2[i] <- s2["last"]
        al1[i] <- fit_alpha_grid(r1, d1, p$w0, p$w1)
        al2[i] <- fit_alpha_grid(r2, d2, p$w0, p$w1)
      }
      icc_slope[iter] <- compute_icc21(sl1, sl2)
      icc_fl[iter]    <- compute_icc21(fl1, fl2)
      icc_last[iter]  <- compute_icc21(la1, la2)
      icc_alpha[iter] <- compute_icc21(al1, al2)
    }

    data.frame(
      n_trials = nt,
      icc_slope_mean = mean(icc_slope, na.rm = TRUE),
      icc_slope_lo   = quantile(icc_slope, 0.025, na.rm = TRUE),
      icc_slope_hi   = quantile(icc_slope, 0.975, na.rm = TRUE),
      icc_fl_mean    = mean(icc_fl, na.rm = TRUE),
      icc_fl_lo      = quantile(icc_fl, 0.025, na.rm = TRUE),
      icc_fl_hi      = quantile(icc_fl, 0.975, na.rm = TRUE),
      icc_last_mean  = mean(icc_last, na.rm = TRUE),
      icc_last_lo    = quantile(icc_last, 0.025, na.rm = TRUE),
      icc_last_hi    = quantile(icc_last, 0.975, na.rm = TRUE),
      icc_alpha_mean = mean(icc_alpha, na.rm = TRUE),
      icc_alpha_lo   = quantile(icc_alpha, 0.025, na.rm = TRUE),
      icc_alpha_hi   = quantile(icc_alpha, 0.975, na.rm = TRUE)
    )
  })
  icc_df <- do.call(rbind, icc_list)

  icc_20 <- icc_df$icc_slope_mean[icc_df$n_trials == 20]
  sb <- data.frame(
    n_trials = trial_counts,
    icc_sb = (trial_counts / 20) * icc_20 /
             (1 + ((trial_counts / 20) - 1) * icc_20)
  )

  results <- list(vd = vd, icc = icc_df, sb = sb)
  saveRDS(results, "../results/variance_decomposition_improved_results.rds")
}
results <- readRDS(vd_cache)
vd <- results$vd; icc_df <- results$icc; sb <- results$sb

# panel a: marginal variance decomposition (grouped bars, normalized marginals not an additive partition)
vd_long <- vd %>%
  select(n_trials, prop_trait, prop_sequence, prop_noise) %>%
  pivot_longer(-n_trials, names_to = "component", values_to = "proportion") %>%
  mutate(component = factor(component,
    levels = c("prop_trait", "prop_sequence", "prop_noise"),
    labels = c("Stable participant differences (\u03B1, w\u2080, w\u2081)",
               "Trial sequence (context)",
               "Observation noise")))

p_a <- ggplot(vd_long, aes(x = factor(n_trials), y = proportion, fill = component)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.7) +
  scale_fill_manual(values = c(
    "Stable participant differences (\u03B1, w\u2080, w\u2081)" = "#1B7837",
    "Trial sequence (context)"    = "#D6604D",
    "Observation noise"           = "#878787"
  )) +
  scale_y_continuous(labels = scales::percent, expand = c(0, 0.01),
                     limits = c(0, 0.72)) +
  labs(x = "Trials per session",
       y = "Relative marginal variance contribution",
       fill = NULL) +
  theme_bw(base_size = 10) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.position = "bottom",
        legend.key.height = unit(0.3, "cm"),
        legend.key.width = unit(0.5, "cm"),
        legend.text = element_text(size = 7.5))

# panel b: ICC vs trial count (behavioral summaries, model, Spearman-Brown)
icc_long <- bind_rows(
  icc_df %>% transmute(n_trials,
    icc = icc_alpha_mean, lo = icc_alpha_lo, hi = icc_alpha_hi,
    method = "Model-based"),
  icc_df %>% transmute(n_trials,
    icc = icc_slope_mean, lo = icc_slope_lo, hi = icc_slope_hi,
    method = "Slope"),
  icc_df %>% transmute(n_trials,
    icc = icc_fl_mean, lo = icc_fl_lo, hi = icc_fl_hi,
    method = "First-last"),
  icc_df %>% transmute(n_trials,
    icc = icc_last_mean, lo = icc_last_lo, hi = icc_last_hi,
    method = "Last trial"),
  sb %>% transmute(n_trials,
    icc = icc_sb, lo = NA_real_, hi = NA_real_,
    method = "Spearman-Brown")
)
icc_long$method <- factor(icc_long$method,
  levels = c("Model-based", "Slope", "First-last", "Last trial", "Spearman-Brown"))

mcol <- c("Model-based" = "#1B7837",
          "Slope" = "#D62728", "First-last" = "#E6550D", "Last trial" = "#756BB1",
          "Spearman-Brown" = "#969696")
mline <- c("Model-based" = "solid",
           "Slope" = "solid", "First-last" = "solid", "Last trial" = "solid",
           "Spearman-Brown" = "dashed")

p_b <- ggplot(icc_long, aes(x = n_trials, y = icc,
                             color = method, linetype = method)) +
  geom_ribbon(aes(ymin = lo, ymax = hi, fill = method),
              alpha = 0.10, color = NA, na.rm = TRUE) +
  geom_line(linewidth = 0.8) +
  geom_point(data = filter(icc_long, !method %in% c("Spearman-Brown")),
             size = 1.5) +
  scale_color_manual(values = mcol, name = NULL) +
  scale_fill_manual(values = mcol, guide = "none") +
  scale_linetype_manual(values = mline, name = NULL) +
  scale_x_log10(breaks = c(20, 50, 100, 200, 500, 1000)) +
  scale_y_continuous(limits = c(-0.1, 1.02), breaks = seq(0, 1, 0.2)) +
  labs(x = "Trials per session (log scale)",
       y = "Test-retest reliability (ICC)") +
  theme_bw(base_size = 10) +
  theme(panel.grid.minor = element_blank(),
        legend.position = "bottom",
        legend.box.margin = margin(t = -4),
        legend.key.height = unit(0.3, "cm"),
        legend.key.width = unit(0.6, "cm"),
        legend.text = element_text(size = 7))

fig <- p_a | p_b
fig <- fig + plot_annotation(tag_levels = "a") &
  theme(plot.tag = element_text(face = "bold", size = 12))

ggsave("../figures/fig2_variance_decomposition.pdf",
       fig, width = 10, height = 5, device = cairo_pdf)

# ---- eight-summary decomposition, all-vary population (Fig 3) ----

# eight behavioral summaries
compute_all_summaries <- function(responses, design) {
  n <- length(responses)
  cs_plus_idx <- which(design$cs_plus == 1)
  cs_minus_idx <- which(design$cs_minus == 1)
  cs_plus_resp <- responses[cs_plus_idx]
  cs_minus_resp <- responses[cs_minus_idx]

  n_csp <- length(cs_plus_resp)
  slope_all <- unname(coef(lm(cs_plus_resp ~ seq_len(n_csp)))[2])
  delta_fl <- cs_plus_resp[n_csp] - cs_plus_resp[1]
  last_trial <- cs_plus_resp[n_csp]
  mean_csplus <- mean(cs_plus_resp)
  mean_csminus <- mean(cs_minus_resp)
  diff_score <- mean_csplus - mean_csminus
  last_block_diff <- mean(tail(cs_plus_resp, 3)) - mean(tail(cs_minus_resp, 3))
  mean_all <- mean(responses)

  c(slope = slope_all, delta_fl = delta_fl, last_trial = last_trial,
    mean_csplus = mean_csplus, mean_csminus = mean_csminus,
    diff_score = diff_score, last_block_diff = last_block_diff,
    mean_all = mean_all)
}

summary_names <- c("slope", "delta_fl", "last_trial", "mean_csplus",
                    "mean_csminus", "diff_score", "last_block_diff", "mean_all")
n_summ <- length(summary_names)

nui_cache <- file.path(res_dir, "nuisance_decomposition_results.rds")
if (!file.exists(nui_cache)) {
  set.seed(20260418)

  N <- 300
  n_trials <- 20
  reinforcement <- 0.8
  n_context <- 200   # context-variance reps
  n_noise_reps <- 100  # noise-variance reps
  n_cond_reps <- 20  # outer averaging reps for trait variances

  participants <- data.frame(
    id    = seq_len(N),
    alpha = runif(N, 0.05, 0.95),
    w0    = pmin(pmax(rnorm(N, -1, 1.5), -5), 4),
    w1    = runif(N, 2, 8)
  )

  # one-at-a-time variance decomposition: vary one source, average over draws of the rest

  full_summaries <- matrix(NA, N * 10, n_summ)
  for (rep in seq_len(10)) {
    for (i in seq_len(N)) {
      p <- participants[i, ]
      des <- make_random_design(n_trials, reinforcement)
      resp <- simulate_session(des, p$alpha, p$w0, p$w1)
      full_summaries[(rep - 1) * N + i, ] <- compute_all_summaries(resp, des)
    }
  }
  colnames(full_summaries) <- summary_names
  total_var <- apply(full_summaries, 2, var)

  var_alpha_reps <- matrix(NA, n_cond_reps, n_summ)
  for (r in seq_len(n_cond_reps)) {
    des_r <- make_random_design(n_trials, reinforcement)
    noise_r <- rnorm(n_trials, 0, 5)
    w0_r <- participants$w0[sample(N, 1)]
    w1_r <- participants$w1[sample(N, 1)]
    alpha_summ <- matrix(NA, N, n_summ)
    for (i in seq_len(N)) {
      resp <- simulate_session(des_r, participants$alpha[i], w0_r, w1_r,
                               noise_draws = noise_r)
      alpha_summ[i, ] <- compute_all_summaries(resp, des_r)
    }
    var_alpha_reps[r, ] <- apply(alpha_summ, 2, var)
  }
  var_alpha <- colMeans(var_alpha_reps)

  var_w0_reps <- matrix(NA, n_cond_reps, n_summ)
  for (r in seq_len(n_cond_reps)) {
    des_r <- make_random_design(n_trials, reinforcement)
    noise_r <- rnorm(n_trials, 0, 5)
    alpha_r <- participants$alpha[sample(N, 1)]
    w1_r <- participants$w1[sample(N, 1)]
    w0_summ <- matrix(NA, N, n_summ)
    for (i in seq_len(N)) {
      resp <- simulate_session(des_r, alpha_r, participants$w0[i], w1_r,
                               noise_draws = noise_r)
      w0_summ[i, ] <- compute_all_summaries(resp, des_r)
    }
    var_w0_reps[r, ] <- apply(w0_summ, 2, var)
  }
  var_w0 <- colMeans(var_w0_reps)

  var_w1_reps <- matrix(NA, n_cond_reps, n_summ)
  for (r in seq_len(n_cond_reps)) {
    des_r <- make_random_design(n_trials, reinforcement)
    noise_r <- rnorm(n_trials, 0, 5)
    alpha_r <- participants$alpha[sample(N, 1)]
    w0_r <- participants$w0[sample(N, 1)]
    w1_summ <- matrix(NA, N, n_summ)
    for (i in seq_len(N)) {
      resp <- simulate_session(des_r, alpha_r, w0_r, participants$w1[i],
                               noise_draws = noise_r)
      w1_summ[i, ] <- compute_all_summaries(resp, des_r)
    }
    var_w1_reps[r, ] <- apply(w1_summ, 2, var)
  }
  var_w1 <- colMeans(var_w1_reps)

  var_context_reps <- matrix(NA, n_cond_reps, n_summ)
  for (r in seq_len(n_cond_reps)) {
    alpha_r <- participants$alpha[sample(N, 1)]
    w0_r <- participants$w0[sample(N, 1)]
    w1_r <- participants$w1[sample(N, 1)]
    noise_r <- rnorm(n_trials, 0, 5)
    ctx_summ <- matrix(NA, n_context, n_summ)
    for (d in seq_len(nrow(ctx_summ))) {
      des <- make_random_design(n_trials, reinforcement)
      resp <- simulate_session(des, alpha_r, w0_r, w1_r,
                               noise_draws = noise_r)
      ctx_summ[d, ] <- compute_all_summaries(resp, des)
    }
    var_context_reps[r, ] <- apply(ctx_summ, 2, var)
  }
  var_context <- colMeans(var_context_reps)

  var_noise_reps <- matrix(NA, n_cond_reps, n_summ)
  for (r in seq_len(n_cond_reps)) {
    alpha_r <- participants$alpha[sample(N, 1)]
    w0_r <- participants$w0[sample(N, 1)]
    w1_r <- participants$w1[sample(N, 1)]
    ref_design <- make_random_design(n_trials, reinforcement)
    noise_summ <- matrix(NA, n_noise_reps, n_summ)
    for (nn in seq_len(nrow(noise_summ))) {
      noise_vec <- rnorm(n_trials, 0, 5)
      resp <- simulate_session(ref_design, alpha_r, w0_r, w1_r,
                               noise_draws = noise_vec)
      noise_summ[nn, ] <- compute_all_summaries(resp, ref_design)
    }
    var_noise_reps[r, ] <- apply(noise_summ, 2, var)
  }
  var_noise <- colMeans(var_noise_reps)

  sum_marginal <- var_alpha + var_w0 + var_w1 + var_context + var_noise
  additivity_ratio <- sum_marginal / total_var
  cat("\nAdditivity check (sum of marginals / total variance):\n")
  for (j in seq_along(summary_names)) {
    cat(sprintf("  %-16s  ratio = %.3f\n", summary_names[j], additivity_ratio[j]))
  }

  raw_total <- sum_marginal
  prop_alpha   <- var_alpha / raw_total
  prop_w0      <- var_w0 / raw_total
  prop_w1      <- var_w1 / raw_total
  prop_context <- var_context / raw_total
  prop_noise   <- var_noise / raw_total

  decomp <- data.frame(
    summary = summary_names,
    alpha = prop_alpha,
    w0 = prop_w0,
    w1 = prop_w1,
    context = prop_context,
    noise = prop_noise
  )

  cat("Variance decomposition:\n")
  print(data.frame(summary = decomp$summary,
                   round(decomp[, -1], 3)))

  # test-retest ICC per summary

  n_icc_iter <- 50
  icc_accum <- matrix(NA, n_icc_iter, length(summary_names))
  colnames(icc_accum) <- summary_names

  for (iter in seq_len(n_icc_iter)) {
    s1_mat <- s2_mat <- matrix(NA, N, length(summary_names))
    for (i in seq_len(N)) {
      des1 <- make_random_design(n_trials, reinforcement)
      des2 <- make_random_design(n_trials, reinforcement)
      r1 <- simulate_session(des1, participants$alpha[i], participants$w0[i],
                             participants$w1[i])
      r2 <- simulate_session(des2, participants$alpha[i], participants$w0[i],
                             participants$w1[i])
      s1_mat[i, ] <- compute_all_summaries(r1, des1)
      s2_mat[i, ] <- compute_all_summaries(r2, des2)
    }
    for (j in seq_along(summary_names)) {
      dd <- data.frame(T1 = s1_mat[, j], T2 = s2_mat[, j])
      dd <- dd[complete.cases(dd), ]
      icc_accum[iter, j] <- tryCatch(ICC(dd, missing = FALSE)$results$ICC[2],
                                      error = function(e) NA)
    }
  }

  icc_results <- colMeans(icc_accum, na.rm = TRUE)
  n_valid <- colSums(!is.na(icc_accum))
  cat("ICC values (valid iterations per summary):\n")
  print(data.frame(ICC = round(icc_results, 3), n_valid = n_valid))

  # validity: |r(summary, true alpha)| averaged over design/noise draws

  n_val_iter <- 30
  val_results <- setNames(numeric(n_summ), summary_names)

  for (iter in seq_len(n_val_iter)) {
    summ_mat <- matrix(NA, N, n_summ)
    for (i in seq_len(N)) {
      des <- make_random_design(n_trials, reinforcement)
      resp <- simulate_session(des, participants$alpha[i], participants$w0[i],
                               participants$w1[i])
      summ_mat[i, ] <- compute_all_summaries(resp, des)
    }
    for (j in seq_along(summary_names)) {
      r_val <- cor(participants$alpha, summ_mat[, j], use = "complete.obs")
      val_results[j] <- val_results[j] + (abs(r_val) / n_val_iter)
    }
  }

  cat("Validity |r(summary, alpha)|:\n")
  print(round(val_results, 3))

  nuisance_results <- list(
    decomposition = decomp,
    icc = icc_results,
    alpha_sens = val_results
  )
  saveRDS(nuisance_results, file.path(res_dir, "nuisance_decomposition_results.rds"))
}
nuisance_results <- readRDS(nui_cache)
decomp <- nuisance_results$decomposition
icc_results <- nuisance_results$icc
val_results <- nuisance_results$alpha_sens

nice_names <- c(
  slope = "Slope", delta_fl = "First-last", last_trial = "Last trial",
  mean_csplus = "Mean CS+", mean_csminus = "Mean CS-",
  diff_score = "CS+ minus CS-", last_block_diff = "End-of-session diff",
  mean_all = "Mean response"
)

plot_df <- decomp %>%
  pivot_longer(-summary, names_to = "source", values_to = "proportion") %>%
  mutate(
    summary = factor(nice_names[summary], levels = rev(nice_names)),
    source = factor(source, levels = c("noise", "context", "w1", "w0", "alpha"))
  )

# scale_fill_manual needs character labels, so re-level with Unicode
level_labels <- c("Noise",
                  "Context (trial sequence)",
                  "w\u2081 (sensitivity)",
                  "w\u2080 (baseline)",
                  "\u03B1 (learning rate)")

plot_df$source <- factor(plot_df$source,
  levels = levels(plot_df$source),
  labels = level_labels)

source_colors <- setNames(
  c("#2ca02c", "#9467bd", "#ff7f0e", "#d62728", "#7f7f7f"),
  rev(level_labels)
)

# variance bars + ICC diamonds, single panel
icc_df <- data.frame(
  summary = factor(nice_names[names(icc_results)], levels = rev(nice_names)),
  icc = as.numeric(icc_results)
)

fig <- ggplot(plot_df, aes(x = proportion, y = summary, fill = source)) +
  geom_col(width = 0.7) +
  geom_point(data = icc_df, aes(x = icc, y = summary),
             inherit.aes = FALSE, shape = 23, size = 3.5, fill = "white",
             color = "black", stroke = 1) +
  scale_fill_manual(values = source_colors, name = "Variance source") +
  scale_x_continuous(limits = c(0, 1.05), breaks = seq(0, 1, 0.25),
                     expand = c(0, 0)) +
  labs(x = "Normalized marginal variance contribution (bars)  /  ICC (diamonds)",
       y = NULL) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  guides(fill = guide_legend(nrow = 2, reverse = TRUE))

ggsave(file.path(fig_dir, "fig3_nuisance_decomposition.pdf"),
       fig, width = 8, height = 5, device = cairo_pdf)
