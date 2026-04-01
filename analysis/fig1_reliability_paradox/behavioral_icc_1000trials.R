# Behavioral ICC at 1000 trials per session.
# Tests whether extending protocols to 1000 trials improves behavioral
# test-retest reliability (behavioral summaries only, no learning rate recovery).

library(tidyverse)

icc_2_1 <- function(x, y) {
  ok <- complete.cases(x, y)
  if (sum(ok) < 3) return(NA_real_)
  x <- x[ok]; y <- y[ok]
  n <- length(x); k <- 2
  dat <- cbind(x, y); gm <- mean(dat)
  SS_s <- k * sum((rowMeans(dat) - gm)^2)
  SS_k <- n * sum((colMeans(dat) - gm)^2)
  SS_e <- sum((dat - gm)^2) - SS_s - SS_k
  MS_s <- SS_s / (n - 1); MS_k <- SS_k / (k - 1); MS_e <- SS_e / ((n - 1) * (k - 1))
  (MS_s - MS_e) / (MS_s + (k - 1) * MS_e + k * (MS_k - MS_e) / n)
}

generate_participants <- function(n = 200) {
  data.frame(
    alpha = runif(n, 0.05, 0.95),
    w0 = pmin(pmax(rnorm(n, -1, 1.5), -5), 4),
    w1 = runif(n, 2, 8)
  )
}

simulate_session <- function(design, alpha, w0, w1, A = 0, K = 100, sigma_y = 5) {
  n <- nrow(design); v_plus <- 0; v_minus <- 0; resp <- numeric(n)
  for (t in seq_len(n)) {
    g <- design$cs_plus[t] * v_plus + design$cs_minus[t] * v_minus
    resp[t] <- A + (K - A) / (1 + exp(-(w0 + w1 * g))) + rnorm(1, 0, sigma_y)
    if (design$cs_plus[t] == 1) v_plus <- v_plus + alpha * (design$us_plus[t] - v_plus)
    if (design$cs_minus[t] == 1) v_minus <- v_minus + alpha * (-1 - v_minus)
  }
  resp
}

set.seed(42)

n_iter <- 200
n_part <- 200
n_trials <- 1000
n_cs_plus <- 500
n_shocks <- 400  # 80% reinforcement

slope_icc <- fl_icc <- lt_icc <- numeric(n_iter)

for (b in seq_len(n_iter)) {
  params <- generate_participants(n_part)
  slope_A <- slope_B <- fl_A <- fl_B <- lt_A <- lt_B <- numeric(n_part)

  for (i in seq_len(n_part)) {
    for (sess in c("A", "B")) {
      cs_seq <- sample(rep(c(1, 0), each = n_trials / 2))
      shocks <- sample(which(cs_seq == 1), n_shocks)
      design <- data.frame(
        cs_plus = cs_seq, cs_minus = 1 - cs_seq,
        us_plus = as.numeric(seq_len(n_trials) %in% shocks)
      )
      resp <- simulate_session(design, params$alpha[i], params$w0[i], params$w1[i])

      sl <- coef(lm(resp ~ seq_len(n_trials)))[2]
      fl <- resp[n_trials] - resp[1]
      lt <- resp[n_trials]

      if (sess == "A") { slope_A[i] <- sl; fl_A[i] <- fl; lt_A[i] <- lt }
      else { slope_B[i] <- sl; fl_B[i] <- fl; lt_B[i] <- lt }
    }
  }

  slope_icc[b] <- icc_2_1(slope_A, slope_B)
  fl_icc[b] <- icc_2_1(fl_A, fl_B)
  lt_icc[b] <- icc_2_1(lt_A, lt_B)
}

results <- data.frame(
  metric = c("slope", "first_last", "last_trial"),
  mean_icc = c(mean(slope_icc), mean(fl_icc), mean(lt_icc)),
  ci_lower = c(quantile(slope_icc, 0.025), quantile(fl_icc, 0.025), quantile(lt_icc, 0.025)),
  ci_upper = c(quantile(slope_icc, 0.975), quantile(fl_icc, 0.975), quantile(lt_icc, 0.975))
)

write.csv(results, "behavioral_icc_1000trials.csv", row.names = FALSE)
