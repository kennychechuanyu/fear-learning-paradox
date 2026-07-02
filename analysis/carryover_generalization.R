# Fig 6: carry-over and generalization. Reliability gradient with gamma=0 and gamma=0.15, then the figure.

library(future.apply)

set.seed(42)

res_dir <- file.path(dirname(getwd()), "results")
fig_dir <- file.path(dirname(getwd()), "figures")

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

N_PARTICIPANTS <- 200
N_BOOTSTRAP    <- 200
N_TRIALS       <- 20
N_SHOCKS       <- 8
A <- 0; K <- 100; SIGMA_Y <- 5
DECAY_FACTORS  <- c(0, 0.2, 0.4, 0.6, 0.8, 1.0)

generate_parameters <- function(n) {
  # fixed w0/w1 to isolate carry-over and generalization on alpha
  data.frame(
    alpha = runif(n, 0.05, 0.95),
    w0    = rep(-1, n),
    w1    = rep(4, n)
  )
}

create_design <- function(n_trials, n_shocks) {
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

compute_ll_gen <- function(alpha, gamma, w0, w1, sigma, responses, design, A, K,
                           v_plus_init = 0, v_minus_init = 0) {
  if (sigma <= 0 || alpha <= 0 || alpha >= 1 || gamma < 0 || gamma >= 0.5) return(-1e10)
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
    vp <- (1 - gamma) * v_plus  + gamma * v_minus
    vm <- (1 - gamma) * v_minus + gamma * v_plus
    v_plus <- vp; v_minus <- vm
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

recover_gen_aware <- function(responses, design, A, K, w0, w1, sigma, estimate_vinit = FALSE) {
  if (estimate_vinit) {
    neg_ll <- function(par) -compute_ll_gen(par[1], par[2], w0, w1, sigma, responses, design, A, K, par[3], par[4])
    starts <- list(c(0.3, 0.10, 0.0, 0.0), c(0.5, 0.20, 0.5, -0.8), c(0.7, 0.05, 0.3, -0.5))
    lower <- c(0.001, 0.0, -0.5, -1.5); upper <- c(0.999, 0.49, 1.5, 0.5)
  } else {
    neg_ll <- function(par) -compute_ll_gen(par[1], par[2], w0, w1, sigma, responses, design, A, K, 0, 0)
    starts <- list(c(0.3, 0.10), c(0.5, 0.20), c(0.7, 0.05))
    lower <- c(0.001, 0.0); upper <- c(0.999, 0.49)
  }
  best_val <- Inf; best_par <- starts[[1]]
  for (s_ in starts) {
    tryCatch({
      opt <- optim(s_, neg_ll, method = "L-BFGS-B", lower = lower, upper = upper,
                   control = list(maxit = 500))
      if (opt$value < best_val) { best_val <- opt$value; best_par <- opt$par }
    }, error = function(e) NULL)
  }
  best_par[1]
}

compute_behavioral <- function(responses, design) {
  csp <- responses[design$cs_plus == 1]
  nc <- length(csp)
  list(
    slope = unname(coef(lm(csp ~ seq_len(nc)))[2]),
    delta_fl = csp[nc] - csp[1],
    last_trial = csp[nc]
  )
}

run_one <- function(iter, n_part, decay, gamma) {
  params <- generate_parameters(n_part)
  naive_A <- naive_B <- correct_B <- numeric(n_part)
  gen_aware_A <- gen_aware_B <- rep(NA_real_, n_part)
  slope_A <- slope_B <- fl_A <- fl_B <- last_A <- last_B <- numeric(n_part)

  for (i in seq_len(n_part)) {
    p <- params[i, ]
    des_A <- create_design(N_TRIALS, N_SHOCKS)
    des_B <- create_design(N_TRIALS, N_SHOCKS)

    sim_A <- simulate_session(des_A, p$alpha, p$w0, p$w1, A, K, SIGMA_Y,
                               gamma = gamma)
    sim_B <- simulate_session(des_B, p$alpha, p$w0, p$w1, A, K, SIGMA_Y,
                               v_plus_init = decay * sim_A$v_plus_final,
                               v_minus_init = decay * sim_A$v_minus_final,
                               gamma = gamma)

    naive_A[i] <- recover_naive(sim_A$responses, des_A, A, K, p$w0, p$w1, SIGMA_Y)
    naive_B[i] <- recover_naive(sim_B$responses, des_B, A, K, p$w0, p$w1, SIGMA_Y)
    correct_B[i] <- recover_correct_carryover(sim_B$responses, des_B, A, K,
                                               p$w0, p$w1, SIGMA_Y)
    if (gamma > 0) {
      gen_aware_A[i] <- recover_gen_aware(sim_A$responses, des_A, A, K, p$w0, p$w1, SIGMA_Y, estimate_vinit = FALSE)
      gen_aware_B[i] <- recover_gen_aware(sim_B$responses, des_B, A, K, p$w0, p$w1, SIGMA_Y, estimate_vinit = TRUE)
    }

    beh_A <- compute_behavioral(sim_A$responses, des_A)
    beh_B <- compute_behavioral(sim_B$responses, des_B)
    slope_A[i] <- beh_A$slope; slope_B[i] <- beh_B$slope
    fl_A[i] <- beh_A$delta_fl; fl_B[i] <- beh_B$delta_fl
    last_A[i] <- beh_A$last_trial; last_B[i] <- beh_B$last_trial
  }

  c(naive_trt = icc_2_1(naive_A, naive_B),
    correct_trt = icc_2_1(naive_A, correct_B),
    gen_aware_trt = icc_2_1(gen_aware_A, gen_aware_B),
    slope_trt = icc_2_1(slope_A, slope_B),
    fl_trt = icc_2_1(fl_A, fl_B),
    last_trt = icc_2_1(last_A, last_B))
}

cache_path <- file.path(res_dir, "carryover_results.rds")
if (!file.exists(cache_path)) {

# gamma = 0
cat("Phase 1: carry-over, no generalization\n")
plan(multisession, workers = max(1, parallel::detectCores() - 1))

all_results_pure <- list()
for (df in DECAY_FACTORS) {
  cat(sprintf("  decay = %.2f, gamma = 0 | %d iterations ... ", df, N_BOOTSTRAP))
  t0 <- Sys.time()
  results <- future_lapply(
    seq_len(N_BOOTSTRAP),
    function(i) run_one(i, N_PARTICIPANTS, df, gamma = 0),
    future.seed = TRUE
  )
  cat(sprintf("done in %.1f sec\n", as.numeric(difftime(Sys.time(), t0, units = "secs"))))

  mat <- do.call(rbind, results)
  beh_avg <- rowMeans(mat[, c("slope_trt", "fl_trt", "last_trt")], na.rm = TRUE)
  summary_df <- data.frame(
    decay_factor = df, gamma = 0,
    metric = c(colnames(mat), "behavioral_avg_trt"),
    mean_r = c(colMeans(mat, na.rm = TRUE), mean(beh_avg, na.rm = TRUE)),
    sd_r = c(apply(mat, 2, sd, na.rm = TRUE), sd(beh_avg, na.rm = TRUE)),
    ci_lower = c(apply(mat, 2, quantile, 0.025, na.rm = TRUE),
                  quantile(beh_avg, 0.025, na.rm = TRUE)),
    ci_upper = c(apply(mat, 2, quantile, 0.975, na.rm = TRUE),
                  quantile(beh_avg, 0.975, na.rm = TRUE)),
    row.names = NULL
  )
  all_results_pure <- c(all_results_pure, list(summary_df))
}
summary_pure <- do.call(rbind, all_results_pure)

# gamma = 0.15
cat("\nPhase 2: carry-over with generalization (gamma = 0.15)\n")

all_results_gen <- list()
for (df in DECAY_FACTORS) {
  cat(sprintf("  decay = %.2f, gamma = 0.15 | %d iterations ... ", df, N_BOOTSTRAP))
  t0 <- Sys.time()
  results <- future_lapply(
    seq_len(N_BOOTSTRAP),
    function(i) run_one(i, N_PARTICIPANTS, df, gamma = 0.15),
    future.seed = TRUE
  )
  cat(sprintf("done in %.1f sec\n", as.numeric(difftime(Sys.time(), t0, units = "secs"))))

  mat <- do.call(rbind, results)
  beh_avg <- rowMeans(mat[, c("slope_trt", "fl_trt", "last_trt")], na.rm = TRUE)
  summary_df <- data.frame(
    decay_factor = df, gamma = 0.15,
    metric = c(colnames(mat), "behavioral_avg_trt"),
    mean_r = c(colMeans(mat, na.rm = TRUE), mean(beh_avg, na.rm = TRUE)),
    sd_r = c(apply(mat, 2, sd, na.rm = TRUE), sd(beh_avg, na.rm = TRUE)),
    ci_lower = c(apply(mat, 2, quantile, 0.025, na.rm = TRUE),
                  quantile(beh_avg, 0.025, na.rm = TRUE)),
    ci_upper = c(apply(mat, 2, quantile, 0.975, na.rm = TRUE),
                  quantile(beh_avg, 0.975, na.rm = TRUE)),
    row.names = NULL
  )
  all_results_gen <- c(all_results_gen, list(summary_df))
}
summary_gen <- do.call(rbind, all_results_gen)

plan(sequential)

carryover_results <- list(
  pure = summary_pure,
  with_gen = summary_gen
)
saveRDS(carryover_results, file.path(res_dir, "carryover_results.rds"))
cat("\nResults saved to:", file.path(res_dir, "carryover_results.rds"), "\n")
cat("Done:", format(Sys.time()), "\n")
}


# panels a,b are noiseless model curves; panel c is the reliability gradient

suppressPackageStartupMessages({library(ggplot2); library(patchwork)})

.sig <- function(g, w0, w1) A + (K - A) / (1 + exp(-(w0 + w1 * g)))
.expected <- function(cs, us, alpha, w0, w1, vp0 = 0, vm0 = 0, gamma = 0) {
  vp <- vp0; vm <- vm0; th <- numeric(length(cs))
  for (t in seq_along(cs)) {
    g <- if (cs[t] == 1) vp else vm
    th[t] <- .sig(g, w0, w1)
    if (cs[t] == 1) vp <- vp + alpha * (us[t] - vp) else vm <- vm + alpha * (-1 - vm)
    if (gamma > 0) { a <- (1-gamma)*vp + gamma*vm; b <- (1-gamma)*vm + gamma*vp; vp <- a; vm <- b }
  }
  list(theta = th, vp = vp, vm = vm)
}
.cue <- function(cs, us, alpha, w0, w1, gamma) {
  vp <- 0; vm <- 0; thp <- rep(NA, length(cs)); thm <- rep(NA, length(cs))
  for (t in seq_along(cs)) {
    if (cs[t]==1){ thp[t] <- .sig(vp,w0,w1); vp <- vp + alpha*(us[t]-vp) } else { thm[t] <- .sig(vm,w0,w1); vm <- vm + alpha*(-1-vm) }
    if (gamma>0){ a<-(1-gamma)*vp+gamma*vm; b<-(1-gamma)*vm+gamma*vp; vp<-a; vm<-b }
  }
  list(p = thp, m = thm)
}

.al <- 0.4; .w0 <- -1; .w1 <- 4
.csp <- rep(1,10); .usp <- rep(1,10)
.s1 <- .expected(.csp,.usp,.al,.w0,.w1)
.s2r <- .expected(.csp,.usp,.al,.w0,.w1, vp0=.s1$vp)
.s2f <- .expected(.csp,.usp,.al,.w0,.w1, vp0=0)
da <- rbind(
  data.frame(x=1:10,  resp=.s1$theta,  grp="Session 1"),
  data.frame(x=12:21, resp=.s2r$theta, grp="Session 2: learning retained"),
  data.frame(x=12:21, resp=.s2f$theta, grp="Session 2: fresh start"))
da$grp <- factor(da$grp, levels=c("Session 1","Session 2: learning retained","Session 2: fresh start"))
p_a <- ggplot(da, aes(x, resp, colour=grp)) +
  annotate("segment", x=11, xend=11, y=12, yend=100, linetype="dashed", colour="grey70", linewidth=0.4) +
  geom_line(linewidth=0.9) + geom_point(size=1.6) +
  annotate("text", x=11, y=6, label="same learning rate", size=2.7, colour="grey45") +
  scale_colour_manual(values=c("Session 1"="grey40","Session 2: learning retained"="#0072B2","Session 2: fresh start"="#E69F00"), name=NULL) +
  scale_x_continuous(breaks=c(1,5,10,12,16,21), labels=c("1","5","10","1","5","10")) +
  scale_y_continuous(limits=c(0,100)) +
  labs(x="CS+ trial (session 1 | session 2)", y="Expected response", title="Memory carry-over") +
  theme_bw(base_size=11) + theme(panel.grid.minor=element_blank(), legend.position="top",
    legend.text=element_text(size=7.6), legend.key.height=unit(0.25,"cm"), legend.box.margin=margin(b=-6),
    plot.title=element_text(size=11,face="bold",hjust=0.5))

.ng <- 16; .csb <- rep(c(1,0), length.out=.ng); .usb <- ifelse(.csb==1,1,0)
.c0 <- .cue(.csb,.usb,.al,.w0,.w1,0); .c1 <- .cue(.csb,.usb,.al,.w0,.w1,0.15)
db <- rbind(
  data.frame(x=1:.ng, resp=.c0$p, cond="No generalization", cue="CS+"),
  data.frame(x=1:.ng, resp=.c0$m, cond="No generalization", cue="CS-"),
  data.frame(x=1:.ng, resp=.c1$p, cond="With generalization", cue="CS+"),
  data.frame(x=1:.ng, resp=.c1$m, cond="With generalization", cue="CS-"))
db <- db[!is.na(db$resp),]
p_b <- ggplot(db, aes(x, resp, colour=cond, linetype=cue)) + geom_line(linewidth=0.9) +
  scale_colour_manual(values=c("No generalization"="#0072B2","With generalization"="#E69F00"), name=NULL) +
  scale_linetype_manual(values=c("CS+"="solid","CS-"="22"), name=NULL) +
  scale_y_continuous(limits=c(0,100)) +
  labs(x="Trial", y="Expected response", title="Generalization between cues") +
  theme_bw(base_size=11) + theme(panel.grid.minor=element_blank(), legend.position="top",
    legend.text=element_text(size=7.6), legend.key.height=unit(0.25,"cm"), legend.box.margin=margin(b=-6),
    plot.title=element_text(size=11,face="bold",hjust=0.5))

co <- readRDS(file.path(res_dir, "carryover_results.rds"))
pc_df <- rbind(
  transform(subset(co$pure,     metric=="correct_trt"),        series="Carry-over-aware"),
  transform(subset(co$with_gen, metric=="gen_aware_trt"),      series="Generalization-aware"),
  transform(subset(co$pure,     metric=="naive_trt"),          series="Naive model (fresh start)"),
  transform(subset(co$with_gen, metric=="naive_trt"),          series="Naive + generalization"),
  transform(subset(co$pure,     metric=="behavioral_avg_trt"), series="Behavioral summaries"))
pc_df$series <- factor(pc_df$series, levels=c("Carry-over-aware","Generalization-aware","Naive model (fresh start)","Naive + generalization","Behavioral summaries"))
.cc <- c("Carry-over-aware"="#0072B2","Generalization-aware"="#009E73","Naive model (fresh start)"="#E69F00","Naive + generalization"="#E69F00","Behavioral summaries"="#D55E00")
.lc <- c("Carry-over-aware"="solid","Generalization-aware"="solid","Naive model (fresh start)"="solid","Naive + generalization"="22","Behavioral summaries"="solid")
p_c <- ggplot(pc_df, aes(decay_factor, mean_r, colour=series, linetype=series, fill=series)) +
  geom_hline(yintercept=0, linetype="dotted", colour="grey70") +
  geom_ribbon(aes(ymin=pmax(ci_lower,-0.05), ymax=ci_upper), alpha=0.15, colour=NA) +
  geom_line(linewidth=1) + geom_point(size=1.6) +
  scale_colour_manual(values=.cc, name=NULL) + scale_fill_manual(values=.cc, guide="none") +
  scale_linetype_manual(values=.lc, name=NULL) + scale_y_continuous(limits=c(-0.05,1)) +
  labs(x="Between-session memory retention", y="Test-retest reliability (ICC)", title="Only the matching repair restores reliability") +
  theme_bw(base_size=11) + theme(panel.grid.minor=element_blank(), legend.position="top",
    legend.text=element_text(size=7.6), legend.key.height=unit(0.25,"cm"), legend.box.margin=margin(b=-6),
    plot.title=element_text(size=11,face="bold",hjust=0.5))

fig5 <- (p_a | p_b) / p_c + plot_annotation(tag_levels="a") & theme(plot.tag=element_text(face="bold",size=13))
ggsave(file.path(fig_dir, "fig6_carryover_gradient.pdf"), fig5, width=9.5, height=8.2, device=cairo_pdf)
cat("Saved fig6_carryover_gradient.pdf\n")
