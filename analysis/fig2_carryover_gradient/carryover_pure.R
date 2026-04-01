# Carry-over gradient without generalization (gamma=0). Generates carry-over results for Fig 2c.

library(future.apply)

# ICC(2,1) — two-way random, absolute agreement, single measures
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

out_dir <- "."

N_PARTICIPANTS <- 200
N_BOOTSTRAP    <- 200
N_TRIALS       <- 20
N_CS_PLUS      <- 10
N_SHOCKS       <- 8
A <- 0; K <- 100; SIGMA_Y <- 5
DECAY_FACTORS  <- c(0, 0.2, 0.4, 0.6, 0.8, 1.0)
GAMMA_TRUE     <- 0  # NO generalization

generate_parameters <- function(n) {
  data.frame(
    alpha = runif(n, 0.05, 0.95),
    w0    = pmin(pmax(rnorm(n, -1, 1.5), -5), 4),
    w1    = runif(n, 2, 8)
  )
}

create_design <- function(n_trials, n_cs_plus, n_shocks) {
  cs_sequence <- sample(rep(c(1, 0), each = n_trials / 2))
  shock_positions <- sample(which(cs_sequence == 1), n_shocks)
  data.frame(
    cs_plus  = cs_sequence,
    cs_minus = 1 - cs_sequence,
    us_plus  = as.numeric(seq_len(n_trials) %in% shock_positions)
  )
}

simulate_session <- function(design, alpha, w0, w1, A, K, sigma_y,
                             v_plus_init = 0, v_minus_init = 0, gamma = 0) {
  n_trials <- nrow(design)
  v_plus <- v_plus_init; v_minus <- v_minus_init
  responses <- numeric(n_trials)
  for (t in seq_len(n_trials)) {
    g <- design$cs_plus[t] * v_plus + design$cs_minus[t] * v_minus
    theta <- A + (K - A) / (1 + exp(-(w0 + w1 * g)))
    responses[t] <- rnorm(1, theta, sigma_y)
    if (design$cs_plus[t] == 1)
      v_plus <- v_plus + alpha * (design$us_plus[t] - v_plus)
    if (design$cs_minus[t] == 1)
      v_minus <- v_minus + alpha * (-1 - v_minus)
    if (gamma > 0) {
      vp <- (1 - gamma) * v_plus  + gamma * v_minus
      vm <- (1 - gamma) * v_minus + gamma * v_plus
      v_plus <- vp; v_minus <- vm
    }
  }
  list(responses = responses, v_plus_final = v_plus, v_minus_final = v_minus)
}

# Recovery: naive (standard RW, V0=0, gamma=0)
compute_ll_std <- function(alpha, w0, w1, sigma, responses, design, A, K,
                           v_plus_init = 0, v_minus_init = 0) {
  if (sigma <= 0 || alpha <= 0 || alpha >= 1) return(-1e10)
  n_trials <- nrow(design)
  v_plus <- v_plus_init; v_minus <- v_minus_init; ll <- 0
  for (t in seq_len(n_trials)) {
    g <- design$cs_plus[t] * v_plus + design$cs_minus[t] * v_minus
    theta <- A + (K - A) / (1 + exp(-(w0 + w1 * g)))
    ll <- ll + dnorm(responses[t], theta, sigma, log = TRUE)
    if (design$cs_plus[t] == 1)
      v_plus <- v_plus + alpha * (design$us_plus[t] - v_plus)
    if (design$cs_minus[t] == 1)
      v_minus <- v_minus + alpha * (-1 - v_minus)
  }
  ll
}

recover_naive <- function(responses, design, A, K, w0, w1, sigma) {
  opt <- optimize(
    function(a) -compute_ll_std(a, w0, w1, sigma, responses, design, A, K, 0, 0),
    interval = c(1e-6, 0.999))
  opt$minimum
}

recover_correct_carryover <- function(responses, design, A, K, w0, w1, sigma) {
  neg_ll <- function(par) {
    -compute_ll_std(par[1], w0, w1, sigma, responses, design, A, K,
                    v_plus_init = par[2], v_minus_init = par[3])
  }
  starts <- list(c(0.3, 0.0, 0.0), c(0.5, 0.5, -0.8), c(0.7, 0.3, -0.5))
  lower <- c(0.001, -0.5, -1.5)
  upper <- c(0.999,  1.5,  0.5)
  best_val <- Inf; best_par <- starts[[1]]
  for (s in starts) {
    tryCatch({
      opt <- optim(s, neg_ll, method = "L-BFGS-B",
                   lower = lower, upper = upper,
                   control = list(maxit = 500))
      if (opt$value < best_val) { best_val <- opt$value; best_par <- opt$par }
    }, error = function(e) NULL)
  }
  best_par[1]
}

compute_behavioral <- function(responses) {
  n <- length(responses)
  list(
    slope = unname(coef(lm(responses ~ seq_len(n)))[2]),
    delta_fl = responses[n] - responses[1],
    last_trial = responses[n]
  )
}

run_one <- function(iter, n_part, decay, gamma) {
  params <- generate_parameters(n_part)
  naive_A <- naive_B <- correct_B <- numeric(n_part)
  slope_A <- slope_B <- fl_A <- fl_B <- last_A <- last_B <- numeric(n_part)

  for (i in seq_len(n_part)) {
    p <- params[i, ]
    des_A <- create_design(N_TRIALS, N_CS_PLUS, N_SHOCKS)
    des_B <- create_design(N_TRIALS, N_CS_PLUS, N_SHOCKS)

    sim_A <- simulate_session(des_A, p$alpha, p$w0, p$w1, A, K, SIGMA_Y,
                               gamma = gamma)
    sim_B <- simulate_session(des_B, p$alpha, p$w0, p$w1, A, K, SIGMA_Y,
                               v_plus_init = decay * sim_A$v_plus_final,
                               v_minus_init = decay * sim_A$v_minus_final,
                               gamma = gamma)

    naive_A[i] <- recover_naive(sim_A$responses, des_A, A, K, p$w0, p$w1, SIGMA_Y)
    naive_B[i] <- recover_naive(sim_B$responses, des_B, A, K, p$w0, p$w1, SIGMA_Y)
    correct_B[i] <- recover_correct_carryover(sim_B$responses, des_B, A, K, p$w0, p$w1, SIGMA_Y)

    beh_A <- compute_behavioral(sim_A$responses)
    beh_B <- compute_behavioral(sim_B$responses)
    slope_A[i] <- beh_A$slope; slope_B[i] <- beh_B$slope
    fl_A[i] <- beh_A$delta_fl; fl_B[i] <- beh_B$delta_fl
    last_A[i] <- beh_A$last_trial; last_B[i] <- beh_B$last_trial
  }

  c(naive_trt = icc_2_1(naive_A, naive_B),
    correct_trt = icc_2_1(naive_A, correct_B),
    slope_trt = icc_2_1(slope_A, slope_B),
    fl_trt = icc_2_1(fl_A, fl_B),
    last_trt = icc_2_1(last_A, last_B))
}

plan(multisession, workers = max(1, parallel::detectCores() - 1))

all_results <- list()
for (df in DECAY_FACTORS) {
  results <- future_lapply(
    seq_len(N_BOOTSTRAP),
    function(i) run_one(i, N_PARTICIPANTS, df, GAMMA_TRUE),
    future.seed = TRUE
  )

  mat <- do.call(rbind, results)
  beh_avg <- rowMeans(mat[, c("slope_trt", "fl_trt", "last_trt")], na.rm = TRUE)

  summary_df <- data.frame(
    decay_factor = df, gamma = GAMMA_TRUE,
    metric = c(colnames(mat), "behavioral_avg_trt"),
    mean_r = c(colMeans(mat, na.rm = TRUE), mean(beh_avg, na.rm = TRUE)),
    sd_r = c(apply(mat, 2, sd, na.rm = TRUE), sd(beh_avg, na.rm = TRUE)),
    ci_lower = c(apply(mat, 2, quantile, 0.025, na.rm = TRUE),
                  quantile(beh_avg, 0.025, na.rm = TRUE)),
    ci_upper = c(apply(mat, 2, quantile, 0.975, na.rm = TRUE),
                  quantile(beh_avg, 0.975, na.rm = TRUE)),
    row.names = NULL
  )
  all_results <- c(all_results, list(summary_df))
}

plan(sequential)
summary_all <- do.call(rbind, all_results)

save(summary_all, file = file.path(out_dir, "pure_simulation_carryover_results.RData"))
