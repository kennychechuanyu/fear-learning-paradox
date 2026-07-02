## Empirical analysis for Fig 8: the pairwise compression coupling between
## behavioral-summary distance and response-trajectory similarity, for the
## fear-acquisition (a) and Iowa Gambling Task (b) datasets. Helper functions,
## data prep, and the two-panel figure in one file.

## Compression-index primitives + significance references (participant-level
## bootstrap CI and a permutation floor).
## Pair order is the column-major lower triangle throughout.

## Pairwise trajectory similarity (Pearson), lower triangle.
traj_sim_vec <- function(traj_mat) {
  C <- cor(t(traj_mat))
  C[lower.tri(C)]
}

## Pairwise summary-space distance (z-scored Euclidean), lower triangle.
summ_dist_vec <- function(coord_mat) {
  D <- as.matrix(dist(scale(coord_mat)))
  D[lower.tri(D)]
}

## Compression index, reusing a precomputed trajectory-similarity vector.
compute_coupling <- function(coord_mat, traj_sim) {
  cor(summ_dist_vec(coord_mat), traj_sim, use = "complete.obs")
}

## Permutation floor (Mantel): permute participant labels, recompute the index.
## Returns observed index, null draws, lower-tail p, and central 95% of the null.
permutation_floor <- function(coord_mat, traj_mat, n_perm = 2000, seed = 7081) {
  set.seed(seed)
  N <- nrow(coord_mat)
  sdist <- summ_dist_vec(coord_mat)        # fixed across permutations
  C <- cor(t(traj_mat))                    # re-labelled by indexing
  lt <- lower.tri(C)
  obs <- cor(sdist, C[lt], use = "complete.obs")
  null <- numeric(n_perm)
  for (b in seq_len(n_perm)) {
    p <- sample(N)
    Cp <- C[p, p]
    null[b] <- cor(sdist, Cp[lt], use = "complete.obs")
  }
  list(obs = obs, null = null,
       p = (1 + sum(null <= obs)) / (n_perm + 1),
       null_ci = quantile(null, c(0.025, 0.975)))
}

## Participant-level bootstrap CI; excludes self-pairs (zero dist, unit similarity).
bootstrap_coupling <- function(coord_mat, traj_mat, n_boot = 2000, seed = 2026) {
  set.seed(seed)
  N <- nrow(coord_mat)
  boot <- numeric(n_boot)
  for (b in seq_len(n_boot)) {
    idx <- sample(N, replace = TRUE)
    D <- as.matrix(dist(scale(coord_mat[idx, ])))
    C <- cor(t(traj_mat[idx, ]))
    keep <- lower.tri(D) & !outer(idx, idx, "==")
    boot[b] <- cor(D[keep], C[keep], use = "complete.obs")
  }
  boot
}

## IGT compression: reads the Iowa Gambling Task data (Steingroever et al. 2015)
## and writes the igt_compression_results.rds cache used by the figure below.

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
})

set.seed(20260416)

DATA_DIR <- "../data"


## IGT: do behavioral summaries capture trial-by-trial dynamics?
## Steingroever et al. (2015), 100-trial studies (N=504).

igt_cache <- "../results/igt_compression_results.rds"
if (!file.exists(igt_cache)) {
  DATA_DIR <- "../data/steingroever2015/IGTdataSteingroever2014"

  choice <- read.csv(file.path(DATA_DIR, "choice_100.csv"), row.names = 1)
  wi     <- read.csv(file.path(DATA_DIR, "wi_100.csv"), row.names = 1)
  lo     <- read.csv(file.path(DATA_DIR, "lo_100.csv"), row.names = 1)

  N <- nrow(choice)
  n_trials <- 100

  ## 5 blocks of 20 trials
  n_blocks <- 5
  block_size <- n_trials / n_blocks

  block_traj <- matrix(NA, N, n_blocks)
  summ_mat   <- matrix(NA, N, 4)
  colnames(summ_mat) <- c("net_score", "net_last20",
                          "learning_slope", "switch_rate")

  for (i in 1:N) {
    ch <- as.numeric(choice[i, ])

    ## advantageous = decks 3 or 4 (C/D)
    adv <- as.numeric(ch %in% c(3, 4))

    ## block-level net scores: (C+D) - (A+B) per block
    for (b in 1:n_blocks) {
      idx <- ((b - 1) * block_size + 1):(b * block_size)
      block_ch <- ch[idx]
      block_traj[i, b] <- sum(block_ch %in% c(3, 4)) - sum(block_ch %in% c(1, 2))
    }

    summ_mat[i, "net_score"]   <- sum(adv) - sum(1 - adv)  # overall net
    summ_mat[i, "net_last20"]  <- sum(adv[81:100]) - sum(1 - adv[81:100])
    summ_mat[i, "learning_slope"] <- unname(coef(lm(block_traj[i, ] ~ seq_len(n_blocks)))[2])
    summ_mat[i, "switch_rate"] <- mean(ch[-1] != ch[-n_trials])
  }

  ## z-score summaries
  summ_z <- scale(summ_mat)

  ## pairwise distances
  n_pairs <- N * (N - 1) / 2

  pair_summ_dist <- numeric(n_pairs)
  pair_traj_cor  <- numeric(n_pairs)

  idx <- 0
  for (i in 1:(N - 1)) {
    for (j in (i + 1):N) {
      idx <- idx + 1
      pair_summ_dist[idx] <- sqrt(sum((summ_z[i, ] - summ_z[j, ])^2, na.rm = TRUE))
      pair_traj_cor[idx]  <- cor(block_traj[i, ], block_traj[j, ])
    }
  }

  overall_r <- cor(pair_summ_dist, pair_traj_cor, use = "complete.obs")

  ## trial-level grain: running proportion of advantageous choices (10-trial window)
  run_prop <- matrix(NA, N, n_trials)
  for (i in 1:N) {
    ch <- as.numeric(choice[i, ])
    adv <- as.numeric(ch %in% c(3, 4))
    for (t in 1:n_trials) {
      win_start <- max(1, t - 9)
      run_prop[i, t] <- mean(adv[win_start:t])
    }
  }

  ## single reported statistic: 4 summaries, run_prop trajectory, full pairs,
  ## via the shared helper so cache/figure/nulls can't drift apart
  tsim_run <- traj_sim_vec(run_prop)
  overall_r_trial <- compute_coupling(summ_mat, tsim_run)
  cat(sprintf("IGT overall_r_trial (4 summaries, run_prop, full pairs) = %.4f\n",
              overall_r_trial))

  results <- list(
    summ_mat = summ_mat,          # 504 x 4 (net_score, net_last20, learning_slope, switch_rate)
    block_traj = block_traj,      # 504 x 5 block net scores (kept for reference)
    run_prop = run_prop,          # 504 x 100 running proportion (the reported trajectory)
    overall_r_block = overall_r,
    overall_r_trial = overall_r_trial,
    N = N
  )
  saveRDS(results, igt_cache)
}

## Fig 8: the two coupling-scatter panels (a, b)

suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(patchwork)
})

N_PERM <- 2000
N_BOOT <- 2000

## Panel a: acquisition summary-trajectory scatter

d <- readRDS("../data/yu2024_npj/Data/data_exclude.rds")
acq <- d[d$phase == "acquisition" & d$stim == d$CSp, ]
acq <- acq[order(acq$participant, acq$trials), ]
participants <- sort(unique(acq$participant))
N_acq <- length(participants)

traj_list <- list()
summ_mat <- matrix(NA, N_acq, 7)
colnames(summ_mat) <- c("mean", "slope", "first_last", "last_trial",
                        "peak", "range", "variance")
for (i in seq_along(participants)) {
  sub <- acq[acq$participant == participants[i], ]
  y <- sub$US_expect; tt <- seq_along(y)
  traj_list[[i]] <- y
  slope <- tryCatch(unname(coef(lm(y ~ tt, na.action = na.exclude))[2]),
                    error = function(e) NA)
  summ_mat[i, ] <- c(mean(y, na.rm = TRUE), slope, y[length(y)] - y[1],
                     y[length(y)], max(y, na.rm = TRUE),
                     max(y, na.rm = TRUE) - min(y, na.rm = TRUE),
                     var(y, na.rm = TRUE))
}
keep <- complete.cases(summ_mat) & sapply(traj_list, function(x) !any(is.na(x)))
cat(sprintf("Excluding %d participants with missing data (keeping %d)\n",
            sum(!keep), sum(keep)))
summ_mat <- summ_mat[keep, ]; traj_list <- traj_list[keep]
N_acq <- sum(keep)
traj_mat_acq <- do.call(rbind, traj_list)          # N_acq x 10

## observed coupling + scatter data
tsim_acq  <- traj_sim_vec(traj_mat_acq)
sdist_acq <- summ_dist_vec(summ_mat)
r_acq <- cor(sdist_acq, tsim_acq, use = "complete.obs")
pair_acq <- data.frame(summ_dist = sdist_acq, traj_cor = tsim_acq)

## bootstrap CI + model-free references
boot_acq <- bootstrap_coupling(summ_mat, traj_mat_acq, N_BOOT, 2026)
ci_acq   <- quantile(boot_acq, c(0.025, 0.975))
perm_acq <- permutation_floor(summ_mat, traj_mat_acq, N_PERM, 7081)
cat(sprintf("ACQ: r=%.3f CI[%.3f,%.3f] perm_p=%.4f\n", r_acq, ci_acq[1], ci_acq[2], perm_acq$p))

p_a <- ggplot(pair_acq, aes(x = summ_dist, y = traj_cor)) +
  geom_point(alpha = 0.12, size = 0.8, color = "grey40") +
  geom_smooth(method = "lm", se = FALSE, color = "black",
              linetype = "dashed", linewidth = 0.5) +
  annotate("text", x = 8.5, y = -0.75,
           label = sprintf("r = %.2f\n[%.2f, %.2f]", r_acq, ci_acq[1], ci_acq[2]),
           size = 3.5, fontface = "bold") +
  scale_x_continuous(limits = c(0, 11)) +
  scale_y_continuous(limits = c(-1, 1)) +
  labs(x = "Pairwise summary distance\n(z-scored Euclidean)",
       y = "Pairwise trial-by-trial\ncorrelation",
       title = sprintf("Fear conditioning (N = %d)", N_acq)) +
  theme_bw(base_size = 11) +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(size = 11, face = "bold", hjust = 0.5))

## Panel b: IGT compression scatter

igt <- readRDS("../results/igt_compression_results.rds")
N_igt <- igt$N

## observed coupling (run_prop trajectory, 4 summaries, full pairs)
tsim_igt  <- traj_sim_vec(igt$run_prop)
sdist_igt <- summ_dist_vec(igt$summ_mat)
r_igt <- cor(sdist_igt, tsim_igt, use = "complete.obs")     # == igt$overall_r_trial
boot_igt <- bootstrap_coupling(igt$summ_mat, igt$run_prop, N_BOOT, 2026)
ci_igt   <- quantile(boot_igt, c(0.025, 0.975))
perm_igt <- permutation_floor(igt$summ_mat, igt$run_prop, N_PERM, 7081)
cat(sprintf("IGT: r=%.3f CI[%.3f,%.3f] perm_p=%.4f\n", r_igt, ci_igt[1], ci_igt[2], perm_igt$p))

## thin for display only; statistics use the full pair set
set.seed(123)
disp <- sample(length(sdist_igt), min(50000, length(sdist_igt)))
pair_igt <- data.frame(summ_dist = sdist_igt[disp], traj_cor = tsim_igt[disp])

p_b <- ggplot(pair_igt, aes(x = summ_dist, y = traj_cor)) +
  geom_point(alpha = 0.04, size = 0.3, color = "grey40") +
  geom_smooth(method = "lm", se = FALSE, color = "black",
              linetype = "dashed", linewidth = 0.5) +
  annotate("text", x = max(pair_igt$summ_dist, na.rm = TRUE) * 0.85, y = -0.75,
           label = sprintf("r = %.2f\n[%.2f, %.2f]", r_igt, ci_igt[1], ci_igt[2]),
           size = 3.5, fontface = "bold") +
  scale_y_continuous(limits = c(-1, 1)) +
  labs(x = "Pairwise summary distance\n(z-scored Euclidean)",
       y = "Pairwise trial-by-trial\ncorrelation",
       title = sprintf("Iowa Gambling Task (N = %d)", N_igt)) +
  theme_bw(base_size = 11) +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(size = 11, face = "bold", hjust = 0.5))


## assemble

fig_main <- (p_a | p_b) +
  plot_annotation(tag_levels = "a") &
  theme(plot.tag = element_text(face = "bold", size = 13))
ggsave("../figures/fig8_empirical_anchor.pdf",
       fig_main, width = 9.5, height = 4.6, device = cairo_pdf)
cat("Saved fig8_empirical_anchor.pdf (a,b scatters)\n")


cat(sprintf("Acquisition r = %.3f, IGT r = %.3f\n", r_acq, r_igt))
