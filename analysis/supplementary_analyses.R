# Supplementary analyses: drift robustness (Supp Fig 1) and empirical-Bayes vs MLE
# (Supp Fig 2). Each section guards its
# simulation behind a cache file in ../results and then plots from that cache.

suppressPackageStartupMessages({
  library(tidyverse)
  library(future.apply)
  library(progressr)
})

res_dir <- file.path(dirname(getwd()), "results")
fig_dir <- file.path(dirname(getwd()), "figures")
dir.create(res_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)


# Supp Fig 1: drift robustness ------------------------------------------------
# Extends the headline reliability-paradox sim by letting Session 2 learning
# rates drift from Session 1 (truncated normal, sigma in {0,.05,.10,.15,.20}).
# One condition: 20 trials, random sequences, 80% reinforcement, fixed w0/w1.
# 50 Monte Carlo iterations, N = 200. Recovery: joint MLE of (alpha, w0, w1).

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

create_random_design <- function(n_trials = 20, n_shocks = 8) {
  cs_sequence <- sample(rep(c(1, 0), each = n_trials / 2))
  cs_plus_trials <- which(cs_sequence == 1)
  shock_positions <- sample(cs_plus_trials, n_shocks)
  data.frame(
    cs_plus = cs_sequence,
    cs_minus = 1 - cs_sequence,
    us_plus = as.numeric(seq_len(n_trials) %in% shock_positions)
  )
}

calculate_behavioral_metrics <- function(responses, cs_plus_indicator) {
  n_trials <- length(responses)
  if (n_trials == 0) {
    return(list(slope = NA_real_, delta_first_last = NA_real_, last_trial = NA_real_))
  }
  csp <- responses[cs_plus_indicator == 1]
  nc <- length(csp)
  metrics <- list()
  if (nc > 2) {
    metrics$slope <- unname(coef(lm(csp ~ seq_len(nc)))[2])
  } else {
    metrics$slope <- NA_real_
  }
  if (nc >= 2) {
    metrics$delta_first_last <- csp[nc] - csp[1]
    metrics$last_trial <- csp[nc]
  } else {
    metrics$delta_first_last <- NA_real_
    metrics$last_trial <- NA_real_
  }
  metrics
}

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

draw_drifted_alpha <- function(alpha1, sigma_drift) {
  if (sigma_drift <= 0) return(alpha1)
  repeat {
    a2 <- rnorm(1, mean = alpha1, sd = sigma_drift)
    if (a2 >= 0.05 && a2 <= 0.95) return(a2)
  }
}

# name keeps "bootstrap" for historical reasons; each iteration draws fresh participants
run_drift_bootstrap <- function(n_participants = 200,
                                n_iterations = 50,
                                drift_levels = c(0, 0.05, 0.10, 0.15, 0.20),
                                n_trials = 20,
                                n_shocks = 8,
                                A_param = 0,
                                K_param = 100,
                                sigma_y = 5,
                                parallel = TRUE) {
  workers <- max(1, parallel::detectCores() - 1)
  previous_plan <- future::plan()
  if (parallel) {
    future::plan(future::multisession, workers = workers)
  } else {
    future::plan(future::sequential)
  }
  on.exit(future::plan(previous_plan), add = TRUE)

  progressr::handlers(global = TRUE)

  results <- list()

  for (sigma_drift in drift_levels) {

    iter_fn <- function(b) {

      participants <- data.frame(
        alpha = runif(n_participants, 0.05, 0.95),
        w0 = rep(-1, n_participants),
        w1 = rep(4, n_participants)
      )

      recovered_A <- numeric(n_participants)
      recovered_B <- numeric(n_participants)
      slope_A <- numeric(n_participants); slope_B <- numeric(n_participants)
      delta_A <- numeric(n_participants); delta_B <- numeric(n_participants)
      last_A  <- numeric(n_participants); last_B  <- numeric(n_participants)

      for (i in seq_len(n_participants)) {
        alpha1 <- participants$alpha[i]
        alpha2 <- draw_drifted_alpha(alpha1, sigma_drift)
        w0 <- participants$w0[i]
        w1 <- participants$w1[i]

        design_A <- create_random_design(n_trials, n_shocks)
        design_B <- create_random_design(n_trials, n_shocks)

        resp_A <- simulate_conditioning_session(design_A, alpha1, w0, w1,
                                                A_param, K_param, sigma_y)
        resp_B <- simulate_conditioning_session(design_B, alpha2, w0, w1,
                                                A_param, K_param, sigma_y)

        recovered_A[i] <- recover_alpha_free_session(resp_A, design_A,
                                                      A_param, K_param, sigma_y)
        recovered_B[i] <- recover_alpha_free_session(resp_B, design_B,
                                                      A_param, K_param, sigma_y)

        bm_A <- calculate_behavioral_metrics(resp_A, design_A$cs_plus)
        bm_B <- calculate_behavioral_metrics(resp_B, design_B$cs_plus)
        slope_A[i] <- bm_A$slope;  slope_B[i] <- bm_B$slope
        delta_A[i] <- bm_A$delta_first_last; delta_B[i] <- bm_B$delta_first_last
        last_A[i]  <- bm_A$last_trial;       last_B[i]  <- bm_B$last_trial
      }

      c(
        alpha            = icc_2_1(recovered_A, recovered_B),
        slope            = icc_2_1(slope_A, slope_B),
        delta_first_last = icc_2_1(delta_A, delta_B),
        last_trial       = icc_2_1(last_A, last_B)
      )
    }

    cat(sprintf("Drift sigma = %.2f ... ", sigma_drift))
    t0 <- Sys.time()
    iter_out <- future.apply::future_lapply(
      seq_len(n_iterations),
      function(b) iter_fn(b),
      future.seed = TRUE
    )
    mat <- do.call(rbind, iter_out)
    cat(sprintf("done (%.1fs)\n", as.numeric(Sys.time() - t0, units = "secs")))

    results[[as.character(sigma_drift)]] <- mat
  }

  results
}

drift_cache <- file.path(res_dir, "drift_robustness_results.RData")
if (!file.exists(drift_cache)) {
  set.seed(20260414)
  t_start <- Sys.time()
  drift_results <- run_drift_bootstrap(
    n_participants = 200,
    n_iterations = 50,
    drift_levels = c(0, 0.05, 0.10, 0.15, 0.20)
  )
  cat(sprintf("Total runtime: %.1f min\n",
              as.numeric(Sys.time() - t_start, units = "mins")))

  summary_df <- purrr::imap_dfr(drift_results, function(mat, sigma_str) {
    sigma_drift <- as.numeric(sigma_str)
    tibble::tibble(
      sigma_drift = sigma_drift,
      metric = colnames(mat),
      mean_icc = apply(mat, 2, mean, na.rm = TRUE),
      sd_icc   = apply(mat, 2, sd,   na.rm = TRUE),
      ci_lo    = apply(mat, 2, quantile, 0.025, na.rm = TRUE),
      ci_hi    = apply(mat, 2, quantile, 0.975, na.rm = TRUE)
    )
  })

  results_wide <- summary_df %>%
    select(sigma_drift, metric, mean_icc) %>%
    tidyr::pivot_wider(names_from = metric, values_from = mean_icc)

  cat("\n=== Drift robustness results (mean ICC across 50 iterations) ===\n")
  print(results_wide, digits = 3)

  cat("\n=== Full summary with 95% CIs ===\n")
  print(summary_df, n = Inf, digits = 3)

  # report post-truncation drift SD and alpha correlation
  cat("\n=== Empirical drift diagnostics (10,000 draws per level) ===\n")
  cat(sprintf("%-12s %12s %12s\n", "nominal_sd", "empirical_sd", "cor(a1,a2)"))
  for (sd_nom in c(0, 0.05, 0.10, 0.15, 0.20)) {
    a1 <- runif(10000, 0.05, 0.95)
    a2 <- sapply(a1, draw_drifted_alpha, sigma_drift = sd_nom)
    cat(sprintf("%-12.2f %12.4f %12.4f\n",
                sd_nom, sd(a2 - a1), cor(a1, a2)))
  }

  save(drift_results, summary_df, results_wide,
       file = file.path(res_dir, "drift_robustness_results.RData"))
  cat("\nSaved drift_robustness_results.RData\n")
}

suppressPackageStartupMessages(library(ggplot2))
.rd <- file.path(dirname(getwd()), "results"); .fd <- file.path(dirname(getwd()), "figures")
.e <- new.env(); load(file.path(.rd, "drift_robustness_results.RData"), envir=.e)
sd_df <- get("summary_df", .e)
.lab <- c(alpha="Recovered learning rate", slope="Slope", delta_first_last="First-last difference", last_trial="Last trial")
sd_df$lab <- factor(.lab[sd_df$metric], levels=.lab)
.cd <- c("Recovered learning rate"="#1B7837","Slope"="#B2182B","First-last difference"="#EF8A62","Last trial"="#E69F00")
p_drift <- ggplot(sd_df, aes(sigma_drift, mean_icc, colour=lab, fill=lab)) +
  geom_hline(yintercept=0.75, linetype="dotted", colour="grey60") +
  geom_ribbon(aes(ymin=ci_lo, ymax=ci_hi), alpha=0.15, colour=NA) +
  geom_line(linewidth=1) + geom_point(size=1.8) +
  scale_colour_manual(values=.cd, name=NULL) + scale_fill_manual(values=.cd, guide="none") +
  scale_y_continuous(limits=c(-0.05,1)) +
  labs(x=expression("Between-session drift ("*sigma[drift]*")"), y="Test-retest reliability (ICC)") +
  theme_bw(base_size=11) + theme(panel.grid.minor=element_blank(), legend.position="bottom")
ggsave(file.path(.fd, "suppfig1_drift_robustness.pdf"), p_drift, width=7, height=5, device=cairo_pdf)
cat("Saved suppfig1_drift_robustness.pdf\n")


# Supp Fig 2: empirical-Bayes MAP shrinkage vs per-participant MLE ------------
# Tests whether plug-in shrinkage (Gaussian prior on logit(alpha) calibrated
# from the MLE distribution) rescues recovery in uninformative design cells,
# or whether recovery tracks the design. 9 cells (3 trial counts x 3 rates),
# 30 Monte Carlo replicates of N=100. Not a full hierarchical posterior.

set.seed(20260417)
suppressPackageStartupMessages({
  library(ggplot2)
  library(patchwork)
})

res_dir <- "../results"
fig_dir <- "../figures"
dir.create(res_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

rw_sim_session <- function(alpha, w0, w1, A, K, sigma_y, design) {
  n_trials <- nrow(design)
  v_plus <- 0; v_minus <- 0
  y <- numeric(n_trials)
  for (t in seq_len(n_trials)) {
    g <- design$cs_plus[t] * v_plus + design$cs_minus[t] * v_minus
    theta <- A + (K - A) / (1 + exp(-(w0 + w1 * g)))
    y[t] <- rnorm(1, theta, sigma_y)
    if (design$cs_plus[t] == 1) v_plus <- v_plus + alpha * (design$us_plus[t] - v_plus)
    if (design$cs_minus[t] == 1) v_minus <- v_minus + alpha * (-1 - v_minus)
  }
  y
}

neg_log_lik <- function(alpha, w0, w1, y, design, A, K, sigma_y) {
  if (alpha <= 0.001 || alpha >= 0.999) return(1e10)
  if (w1 <= 0) return(1e10)
  n <- nrow(design); v_plus <- 0; v_minus <- 0; nll <- 0
  for (t in seq_len(n)) {
    g <- design$cs_plus[t] * v_plus + design$cs_minus[t] * v_minus
    theta <- A + (K - A) / (1 + exp(-(w0 + w1 * g)))
    nll <- nll - dnorm(y[t], theta, sigma_y, log = TRUE)
    if (design$cs_plus[t] == 1)  v_plus  <- v_plus  + alpha * (design$us_plus[t] - v_plus)
    if (design$cs_minus[t] == 1) v_minus <- v_minus + alpha * (-1 - v_minus)
  }
  nll
}

fit_mle <- function(y, design, A, K, sigma_y) {
  starts <- list(c(0.3, -1, 4), c(0.5, 0, 5), c(0.7, -2, 3))
  best <- list(val = Inf)
  for (s in starts) {
    opt <- tryCatch(optim(
      par = s,
      fn = function(p) neg_log_lik(p[1], p[2], p[3], y, design, A, K, sigma_y),
      method = "L-BFGS-B",
      lower = c(0.001, -5, 0.5), upper = c(0.999, 4, 15)
    ), error = function(e) list(par = c(NA, NA, NA), value = Inf, convergence = 1))
    if (opt$value < best$val) best <- list(par = opt$par, val = opt$value, conv = opt$convergence)
  }
  list(alpha = best$par[1], w0 = best$par[2], w1 = best$par[3], converged = !is.null(best$conv) && best$conv == 0)
}

# plug-in empirical-Bayes MAP: Gaussian prior on logit(alpha) from population MLE
fit_ebayes <- function(y, design, A, K, sigma_y, mu_logit, sd_logit) {
  neg_log_post <- function(params) {
    alpha <- params[1]; w0 <- params[2]; w1 <- params[3]
    if (alpha <= 0.001 || alpha >= 0.999) return(1e10)
    if (w1 <= 0) return(1e10)
    nll <- neg_log_lik(alpha, w0, w1, y, design, A, K, sigma_y)
    lp <- qlogis(alpha)
    nlp <- 0.5 * ((lp - mu_logit) / sd_logit)^2
    nll + nlp
  }
  starts <- list(c(0.3, -1, 4), c(0.5, 0, 5), c(0.7, -2, 3))
  best <- list(val = Inf)
  for (s in starts) {
    opt <- tryCatch(optim(
      par = s, fn = neg_log_post, method = "L-BFGS-B",
      lower = c(0.001, -5, 0.5), upper = c(0.999, 4, 15)
    ), error = function(e) list(par = c(NA, NA, NA), value = Inf, convergence = 1))
    if (opt$value < best$val) best <- list(par = opt$par, val = opt$value, conv = opt$convergence)
  }
  list(alpha = best$par[1], w0 = best$par[2], w1 = best$par[3])
}

make_design <- function(n_csplus, reinforcement_rate) {
  # matched 1:1 CS+/CS- randomized order, randomized reinforcement
  n_csminus <- n_csplus
  n_trials <- n_csplus + n_csminus
  cs_sequence <- sample(rep(c(1, 0), each = n_csplus))
  n_reinforced <- round(reinforcement_rate * n_csplus)
  csplus_idx <- which(cs_sequence == 1)
  reinforced_idx <- sample(csplus_idx, n_reinforced)
  us_plus <- rep(0, n_trials); us_plus[reinforced_idx] <- 1
  data.frame(
    cs_plus  = cs_sequence,
    cs_minus = 1 - cs_sequence,
    us_plus  = us_plus,
    us_minus = rep(0, n_trials)
  )
}

N       <- 100
A <- 0; K <- 100; sigma_y <- 5

cells <- expand.grid(
  n_csplus = c(5, 15, 30),             # 10, 30, 60 total trials
  rate     = c(0.30, 0.50, 1.00),
  stringsAsFactors = FALSE
)

run_cell <- function(n_csplus, rate) {
  actual_n_reinforced <- round(rate * n_csplus)
  actual_rate <- actual_n_reinforced / n_csplus

  alpha_true <- runif(N, 0.05, 0.95)
  w0_true    <- pmin(pmax(rnorm(N, -1, 1.5), -5), 4)
  w1_true    <- runif(N, 2, 8)

  y_mat <- matrix(NA, N, 2 * n_csplus)
  designs <- vector("list", N)
  for (i in 1:N) {
    d <- make_design(n_csplus, rate)
    designs[[i]] <- d
    y_mat[i, ] <- rw_sim_session(alpha_true[i], w0_true[i], w1_true[i], A, K, sigma_y, d)
  }

  # MLE per participant
  mle <- vector("list", N)
  for (i in 1:N) mle[[i]] <- fit_mle(y_mat[i, ], designs[[i]], A, K, sigma_y)
  alpha_mle <- sapply(mle, function(f) f$alpha)

  # calibrate EB prior from MLE population
  alpha_mle_clipped <- pmin(pmax(alpha_mle, 0.02), 0.98)
  logit_mle <- qlogis(alpha_mle_clipped)
  mu_logit  <- mean(logit_mle, na.rm = TRUE)
  sd_logit  <- max(sd(logit_mle, na.rm = TRUE), 1.0)

  # EB MAP per participant
  eb <- vector("list", N)
  for (i in 1:N) eb[[i]] <- fit_ebayes(y_mat[i, ], designs[[i]], A, K, sigma_y, mu_logit, sd_logit)
  alpha_eb <- sapply(eb, function(f) f$alpha)

  ok_mle <- !is.na(alpha_mle)
  ok_eb  <- !is.na(alpha_eb)
  r2_mle <- if (sum(ok_mle) > 2) cor(alpha_true[ok_mle], alpha_mle[ok_mle])^2 else NA
  r2_eb  <- if (sum(ok_eb)  > 2) cor(alpha_true[ok_eb],  alpha_eb[ok_eb])^2  else NA

  rmse_mle <- if (sum(ok_mle) > 2) sqrt(mean((alpha_mle[ok_mle] - alpha_true[ok_mle])^2)) else NA
  rmse_eb  <- if (sum(ok_eb)  > 2) sqrt(mean((alpha_eb[ok_eb]  - alpha_true[ok_eb])^2))  else NA
  mae_mle  <- if (sum(ok_mle) > 2) mean(abs(alpha_mle[ok_mle] - alpha_true[ok_mle])) else NA
  mae_eb   <- if (sum(ok_eb)  > 2) mean(abs(alpha_eb[ok_eb]  - alpha_true[ok_eb]))  else NA

  list(n_csplus = n_csplus, rate = rate, actual_rate = actual_rate,
       r2_mle = r2_mle, r2_eb = r2_eb,
       rmse_mle = rmse_mle, rmse_eb = rmse_eb,
       mae_mle = mae_mle, mae_eb = mae_eb)
}

hbayes_cache <- file.path(res_dir, "hbayes_vs_mle_results.rds")
if (!file.exists(hbayes_cache)) {
  n_rep <- 30
  cat(sprintf("Running %d replicates x %d cells ...\n", n_rep, nrow(cells)))

  all_results <- list()
  for (rep in seq_len(n_rep)) {
    for (k in seq_len(nrow(cells))) {
      cat(sprintf("  Cell %d/%d: %d CS+, %.0f%% ... ",
                  k, nrow(cells), cells$n_csplus[k], cells$rate[k] * 100))
      r <- run_cell(cells$n_csplus[k], cells$rate[k])
      r$rep <- rep
      all_results[[length(all_results) + 1]] <- r
      cat(sprintf("R²_MLE=%.3f R²_EB=%.3f ΔR²=%+.3f RMSE_MLE=%.3f RMSE_EB=%.3f\n",
                  r$r2_mle, r$r2_eb, r$r2_eb - r$r2_mle, r$rmse_mle, r$rmse_eb))
    }
  }

  res_df <- do.call(rbind, lapply(all_results, function(r) {
    data.frame(rep = r$rep, n_csplus = r$n_csplus, rate = r$rate,
               actual_rate = r$actual_rate,
               r2_mle = r$r2_mle, r2_eb = r$r2_eb,
               delta_r2 = r$r2_eb - r$r2_mle,
               rmse_mle = r$rmse_mle, rmse_eb = r$rmse_eb,
               mae_mle = r$mae_mle, mae_eb = r$mae_eb)
  }))

  cat("\n=== Summary (mean across", n_rep, "replicates) ===\n")
  for (k in seq_len(nrow(cells))) {
    sub <- res_df[res_df$n_csplus == cells$n_csplus[k] & res_df$rate == cells$rate[k], ]
    cat(sprintf("%2d CS+ / %3.0f%% (actual %.0f%%): R²_MLE=%.3f R²_EB=%.3f ΔR²=%+.4f RMSE_MLE=%.3f RMSE_EB=%.3f\n",
                cells$n_csplus[k], cells$rate[k]*100, sub$actual_rate[1]*100,
                mean(sub$r2_mle), mean(sub$r2_eb), mean(sub$delta_r2),
                mean(sub$rmse_mle), mean(sub$rmse_eb)))
  }

  cat(sprintf("\nMax mean ΔR²: %+.4f\n", max(aggregate(delta_r2 ~ n_csplus + rate, res_df, mean)$delta_r2)))

  saveRDS(res_df, file.path(res_dir, "hbayes_vs_mle_results.rds"))
  cat("Saved to hbayes_vs_mle_results.rds\n")
}
res_df <- readRDS(hbayes_cache)

agg <- aggregate(
  cbind(r2_mle, r2_eb, delta_r2) ~ n_csplus + rate,
  data = res_df, FUN = mean
)

# label by CS+ count, not total trials (2 * n_csplus)
agg$cell_label <- sprintf("%d CS+ / %d%%", agg$n_csplus, round(agg$rate * 100))

agg <- agg[order(agg$r2_mle), ]
agg$cell_label <- factor(agg$cell_label, levels = agg$cell_label)

agg_long <- rbind(
  data.frame(cell_label = agg$cell_label, method = "Naive MLE",
             r2 = agg$r2_mle, stringsAsFactors = FALSE),
  data.frame(cell_label = agg$cell_label, method = "Empirical Bayes",
             r2 = agg$r2_eb, stringsAsFactors = FALSE)
)
agg_long$method <- factor(agg_long$method, levels = c("Naive MLE", "Empirical Bayes"))

p_a <- ggplot(agg_long, aes(x = cell_label, y = r2, fill = method)) +
  geom_col(position = position_dodge(0.8), width = 0.7) +
  geom_hline(yintercept = 0.9, linetype = "dashed", colour = "grey30") +
  scale_fill_manual(values = c("Naive MLE" = "#D55E00", "Empirical Bayes" = "#0072B2")) +
  scale_y_continuous(limits = c(0, 1.05), breaks = seq(0, 1, 0.2)) +
  labs(
    title = "Recovery across design cells",
    y = expression(paste("Recovery  ", R^2)),
    x = "Design cell (trials / reinforcement rate)",
    fill = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    panel.grid.major.x = element_blank(),
    legend.position = "top"
  )

p_b <- ggplot(agg, aes(x = cell_label, y = delta_r2,
                        fill = ifelse(delta_r2 >= 0, "pos", "neg"))) +
  geom_col(width = 0.6, show.legend = FALSE) +
  geom_hline(yintercept = 0, colour = "grey30") +
  scale_fill_manual(values = c("pos" = "#0072B2", "neg" = "#D55E00")) +
  scale_y_continuous(limits = c(-0.06, 0.16), breaks = seq(-0.05, 0.15, 0.05)) +
  labs(
    title = "Partial-pooling gain is small everywhere",
    y = expression(paste(Delta, R^2, "  (EBayes ", - "", " MLE)")),
    x = "Design cell (trials / reinforcement rate)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    panel.grid.major.x = element_blank()
  )

fig <- p_a / p_b +
  plot_annotation(tag_levels = "a",
                  tag_prefix = "", tag_suffix = "",
                  theme = theme(plot.tag = element_text(face = "bold", size = 14)))

ggsave(file.path(fig_dir, "suppfig2_hbayes_vs_mle.pdf"), fig,
       width = 9, height = 10, dpi = 300)
ggsave(file.path(fig_dir, "suppfig2_hbayes_vs_mle.png"), fig,
       width = 9, height = 10, dpi = 300)

cat("Figure saved to suppfig2_hbayes_vs_mle.pdf/png\n")
