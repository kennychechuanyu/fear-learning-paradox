# =============================================================================
# Figure 2: Three panels — carry-over, generalization, simulation
# Nature Neuroscience style: Helvetica, no gridlines, clean axes
# =============================================================================

library(ggplot2)
library(patchwork)

base_dir <- "/Users/kenny/Library/CloudStorage/GoogleDrive-kennyyu0924@gmail.com/My Drive/Work/reliability/proof of concept"
out_dir  <- file.path(base_dir, "source 2")

load(file.path(out_dir, "pure_simulation_carryover_results.RData"))
load(file.path(out_dir, "carryover_with_gen_results.RData"))

col_blue       <- "#0072B2"
col_orange     <- "#E69F00"
col_vermillion <- "#D55E00"
col_gray       <- "gray45"

sigmoid <- function(v, w0, w1, A, K) A + (K - A) / (1 + exp(-(w0 + w1 * v)))

A <- 0; K <- 100

# Shared theme: Nature style
theme_nature <- function() {
  theme_minimal(base_size = 9, base_family = "Helvetica") %+replace%
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_line(color = "gray30", linewidth = 0.4),
      axis.ticks = element_line(color = "gray30", linewidth = 0.3),
      axis.ticks.length = unit(0.12, "cm"),
      plot.margin = margin(5, 5, 5, 5)
    )
}

# =============================================================================
# Panel a: Carry-over example
# =============================================================================
ex_alpha <- 0.15; ex_w0 <- -1.0; ex_w1 <- 5.0; n_csplus <- 10
us_schedule <- c(0, 0, 1, 1, 1, 1, 1, 1, 1, 1)

v_s1 <- numeric(n_csplus); v <- 0
for (t in 1:n_csplus) { v_s1[t] <- v; v <- v + ex_alpha * (us_schedule[t] - v) }
v_final <- v

v_no <- numeric(n_csplus); v <- 0
for (t in 1:n_csplus) { v_no[t] <- v; v <- v + ex_alpha * (us_schedule[t] - v) }

v_full <- numeric(n_csplus); v <- v_final
for (t in 1:n_csplus) { v_full[t] <- v; v <- v + ex_alpha * (us_schedule[t] - v) }

resp_s1   <- sigmoid(v_s1, ex_w0, ex_w1, A, K)
resp_no   <- sigmoid(v_no, ex_w0, ex_w1, A, K)
resp_full <- sigmoid(v_full, ex_w0, ex_w1, A, K)

x_s1 <- 1:n_csplus; x_s2 <- 13:22

df_a <- rbind(
  data.frame(x = x_s1, y = resp_s1, us = factor(us_schedule), grp = "s1"),
  data.frame(x = x_s2, y = resp_full, us = factor(us_schedule), grp = "full"),
  data.frame(x = x_s2, y = resp_no, us = factor(us_schedule), grp = "no")
)

p_a <- ggplot(df_a, aes(x = x, y = y)) +
  # Vertical session separator
  geom_vline(xintercept = 11.5, linetype = "dashed", color = "gray80", linewidth = 0.3) +
  # Session 1
  geom_line(data = df_a[df_a$grp == "s1", ], color = col_gray, linewidth = 0.9) +
  geom_point(data = df_a[df_a$grp == "s1", ], aes(shape = us),
             color = col_gray, fill = "white", size = 2.0, stroke = 0.7) +
  # Dotted connectors: blue (carry-over) and orange (memory loss)
  geom_line(data = data.frame(x = c(10, 13), y = c(resp_s1[10], resp_full[1])),
            color = col_blue, linewidth = 0.7, linetype = "dotted") +
  geom_line(data = data.frame(x = c(10, 13), y = c(resp_s1[10], resp_no[1])),
            color = col_orange, linewidth = 0.7, linetype = "dotted") +
  # Session 2: full retention (blue)
  geom_line(data = df_a[df_a$grp == "full", ], color = col_blue, linewidth = 0.9) +
  geom_point(data = df_a[df_a$grp == "full", ], aes(shape = us),
             color = col_blue, fill = "white", size = 2.0, stroke = 0.7) +
  # Session 2: no retention (orange)
  geom_line(data = df_a[df_a$grp == "no", ], color = col_orange, linewidth = 0.9) +
  geom_point(data = df_a[df_a$grp == "no", ], aes(shape = us),
             color = col_orange, fill = "white", size = 2.0, stroke = 0.7) +
  scale_shape_manual(name = NULL, values = c("0" = 21, "1" = 16),
                     labels = c("0" = "No shock", "1" = "Shock")) +
  # Session labels
  annotate("text", x = 1, y = 98, label = "Session 1", hjust = 0,
           size = 3.0, fontface = "bold", color = "gray30", family = "Helvetica") +
  annotate("text", x = 13, y = 98, label = "Session 2", hjust = 0,
           size = 3.0, fontface = "bold", color = "gray30", family = "Helvetica") +
  # Direct labels
  annotate("text", x = 22.7, y = resp_full[10], label = "Full retention",
           color = col_blue, hjust = 0, size = 2.5, fontface = "bold", family = "Helvetica") +
  annotate("text", x = 22.7, y = resp_no[10], label = "No retention",
           color = col_orange, hjust = 0, size = 2.5, fontface = "bold", family = "Helvetica") +
  annotate("text", x = 11.5, y = 14,
           label = "Same learning rate",
           size = 2.8, color = "gray40") +
  scale_x_continuous(name = "CS+ trial",
                     breaks = c(1, 5, 10, 13, 17, 22),
                     labels = c("1", "5", "10", "1", "5", "10")) +
  scale_y_continuous(name = "Expected response", limits = c(10, 100)) +
  coord_cartesian(clip = "off") +
  theme_nature() +
  theme(
    plot.margin = margin(5, 45, 5, 5),
    legend.position = "bottom",
    legend.key.size = unit(0.35, "cm"),
    legend.text = element_text(size = 7),
    legend.margin = margin(t = -5, b = 0)
  ) +
  guides(shape = guide_legend(override.aes = list(color = "gray40", size = 2.2)))

# =============================================================================
# Panel b: Generalization example — interleaved CS+/CS-
# =============================================================================
n_tr <- 16
cs_seq <- rep(c(1, 0), 8)
us_b <- rep(0, n_tr)
us_b[which(cs_seq == 1)[2:8]] <- 1

gen_alpha <- 0.30; gen_w0 <- -1.0; gen_w1 <- 5.0

# No generalization
v_no_b <- numeric(n_tr); vp <- 0; vm <- 0
for (t in 1:n_tr) {
  v_no_b[t] <- if (cs_seq[t] == 1) vp else vm
  if (cs_seq[t] == 1) vp <- vp + gen_alpha * (us_b[t] - vp)
  if (cs_seq[t] == 0) vm <- vm + gen_alpha * (-1 - vm)
}

# With generalization
gamma_b <- 0.15
v_gen_b <- numeric(n_tr); vp <- 0; vm <- 0
for (t in 1:n_tr) {
  v_gen_b[t] <- if (cs_seq[t] == 1) vp else vm
  if (cs_seq[t] == 1) vp <- vp + gen_alpha * (us_b[t] - vp)
  if (cs_seq[t] == 0) vm <- vm + gen_alpha * (-1 - vm)
  vp_new <- (1 - gamma_b) * vp + gamma_b * vm
  vm_new <- (1 - gamma_b) * vm + gamma_b * vp
  vp <- vp_new; vm <- vm_new
}

resp_no_b  <- sigmoid(v_no_b, gen_w0, gen_w1, A, K)
resp_gen_b <- sigmoid(v_gen_b, gen_w0, gen_w1, A, K)

cs_label <- ifelse(cs_seq == 1, "CS+", "CS-")
df_b <- rbind(
  data.frame(trial = 1:n_tr, resp = resp_no_b, cs = cs_label,
             cond = "no", cs_type = cs_seq, stringsAsFactors = FALSE),
  data.frame(trial = 1:n_tr, resp = resp_gen_b, cs = cs_label,
             cond = "gen", cs_type = cs_seq, stringsAsFactors = FALSE)
)

no_plus   <- df_b[df_b$cond == "no"  & df_b$cs_type == 1, ]
no_minus  <- df_b[df_b$cond == "no"  & df_b$cs_type == 0, ]
gen_plus  <- df_b[df_b$cond == "gen" & df_b$cs_type == 1, ]
gen_minus <- df_b[df_b$cond == "gen" & df_b$cs_type == 0, ]

# Right-edge y positions for direct labels
y_no_plus   <- tail(no_plus$resp, 1)
y_no_minus  <- tail(no_minus$resp, 1)
y_gen_plus  <- tail(gen_plus$resp, 1)
y_gen_minus <- tail(gen_minus$resp, 1)

p_b <- ggplot() +
  # Lines: solid = CS+, dashed = CS-
  geom_line(data = no_plus, aes(x = trial, y = resp),
            color = col_blue, linewidth = 0.7) +
  geom_line(data = no_minus, aes(x = trial, y = resp),
            color = col_blue, linewidth = 0.7, linetype = "dashed") +
  geom_line(data = gen_plus, aes(x = trial, y = resp),
            color = col_orange, linewidth = 0.7) +
  geom_line(data = gen_minus, aes(x = trial, y = resp),
            color = col_orange, linewidth = 0.7, linetype = "dashed") +
  # Points: circles = CS+, triangles = CS-
  geom_point(data = no_plus, aes(x = trial, y = resp),
             color = col_blue, size = 2.0, shape = 16) +
  geom_point(data = no_minus, aes(x = trial, y = resp),
             color = col_blue, size = 2.0, shape = 17) +
  geom_point(data = gen_plus, aes(x = trial, y = resp),
             color = col_orange, size = 2.0, shape = 16) +
  geom_point(data = gen_minus, aes(x = trial, y = resp),
             color = col_orange, size = 2.0, shape = 17) +
  # Direct labels at right edge
  annotate("text", x = n_tr + 0.6, y = y_no_plus + 2,
           label = "CS+", color = col_blue, hjust = 0,
           size = 2.5, fontface = "bold", family = "Helvetica") +
  annotate("text", x = n_tr + 0.6, y = y_no_minus,
           label = "CS\u2013", color = col_blue, hjust = 0,
           size = 2.5, fontface = "bold", family = "Helvetica") +
  annotate("text", x = n_tr + 0.6, y = y_gen_plus + 2,
           label = "CS+", color = col_orange, hjust = 0,
           size = 2.5, fontface = "bold", family = "Helvetica") +
  annotate("text", x = n_tr + 0.6, y = y_gen_minus,
           label = "CS\u2013", color = col_orange, hjust = 0,
           size = 2.5, fontface = "bold", family = "Helvetica") +
  # Condition labels at top
  annotate("text", x = 1, y = 100, label = "No generalization",
           color = col_blue, hjust = 0, size = 2.5, fontface = "bold",
           family = "Helvetica") +
  annotate("text", x = 1, y = 94, label = "With generalization",
           color = col_orange, hjust = 0, size = 2.5, fontface = "bold",
           family = "Helvetica") +
  # Identical alpha
  annotate("text", x = 8.5, y = -8,
           label = "Same learning rate",
           size = 2.8, color = "gray40") +
  scale_x_continuous(name = "Trial",
                     breaks = c(1, 4, 8, 12, 16),
                     limits = c(0.3, n_tr + 3.5)) +
  scale_y_continuous(name = "Expected response", limits = c(-12, 102)) +
  coord_cartesian(clip = "off") +
  theme_nature() +
  theme(
    plot.margin = margin(5, 5, 5, 5)
  )

# =============================================================================
# Panel c: Simulation results
# =============================================================================
d_correct    <- summary_all[summary_all$metric == "correct_trt", ]
d_naive      <- summary_all[summary_all$metric == "naive_trt", ]
d_behavioral <- summary_all[summary_all$metric == "behavioral_avg_trt", ]
d_behavioral$ci_lower_clip <- pmax(d_behavioral$ci_lower, -0.15)
d_naive_gen  <- summary_gen[summary_gen$metric == "naive_trt", ]

y_correct    <- d_correct[d_correct$decay_factor == 1, "mean_r"]
y_naive      <- d_naive[d_naive$decay_factor == 1, "mean_r"]
y_behavioral <- d_behavioral[d_behavioral$decay_factor == 1, "mean_r"]
y_naive_gen  <- d_naive_gen[d_naive_gen$decay_factor == 1, "mean_r"]

p_c <- ggplot() +
  # Zero reference
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray60", linewidth = 0.4) +
  # Confidence ribbons
  geom_ribbon(data = d_correct, aes(x = decay_factor, ymin = ci_lower, ymax = ci_upper),
              fill = col_blue, alpha = 0.15) +
  geom_ribbon(data = d_naive, aes(x = decay_factor, ymin = ci_lower, ymax = ci_upper),
              fill = col_orange, alpha = 0.15) +
  geom_ribbon(data = d_naive_gen, aes(x = decay_factor, ymin = ci_lower, ymax = ci_upper),
              fill = col_orange, alpha = 0.08) +
  geom_ribbon(data = d_behavioral, aes(x = decay_factor, ymin = ci_lower_clip, ymax = ci_upper),
              fill = col_vermillion, alpha = 0.15) +
  # Correct model (blue)
  geom_line(data = d_correct, aes(x = decay_factor, y = mean_r),
            color = col_blue, linewidth = 1.0) +
  geom_point(data = d_correct, aes(x = decay_factor, y = mean_r),
             color = col_blue, size = 2.2, shape = 16) +
  # Naive model (orange solid, open circles)
  geom_line(data = d_naive, aes(x = decay_factor, y = mean_r),
            color = col_orange, linewidth = 1.0) +
  geom_point(data = d_naive, aes(x = decay_factor, y = mean_r),
             color = col_orange, size = 2.2, shape = 21, fill = "white", stroke = 1) +
  # Naive + generalization (orange dashed, X markers)
  geom_line(data = d_naive_gen, aes(x = decay_factor, y = mean_r),
            color = col_orange, linewidth = 1.0, linetype = "dashed") +
  geom_point(data = d_naive_gen, aes(x = decay_factor, y = mean_r),
             color = col_orange, size = 2.2, shape = 4, stroke = 1) +
  # Behavioral (vermillion, thicker, square markers for distinction)
  geom_line(data = d_behavioral, aes(x = decay_factor, y = mean_r),
            color = col_vermillion, linewidth = 1.2) +
  geom_point(data = d_behavioral, aes(x = decay_factor, y = mean_r),
             color = col_vermillion, size = 2.2, shape = 15) +
  # Direct labels
  annotate("text", x = 1.03, y = y_correct, label = "Correct model",
           color = col_blue, hjust = 0, size = 2.5, fontface = "bold",
           family = "Helvetica") +
  annotate("text", x = 1.03, y = y_naive, label = "Naive model",
           color = col_orange, hjust = 0, size = 2.5, fontface = "bold",
           family = "Helvetica") +
  annotate("text", x = 1.03, y = y_naive_gen - 0.02,
           label = "Naive + generalization",
           color = col_orange, hjust = 0, size = 2.2, fontface = "italic",
           family = "Helvetica") +
  annotate("text", x = 1.03, y = y_behavioral, label = "Behavioral",
           color = col_vermillion, hjust = 0, size = 2.3, fontface = "bold",
           family = "Helvetica") +
  scale_x_continuous(name = "Between-session memory retention",
                     breaks = seq(0, 1, 0.2),
                     limits = c(-0.02, 1.42), expand = c(0, 0)) +
  scale_y_continuous(name = "Test-retest reliability (ICC)",
                     breaks = seq(0, 1, 0.2),
                     limits = c(-0.15, 1.05), expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  theme_nature() +
  theme(plot.margin = margin(5, 45, 5, 5))

# =============================================================================
# Combine: top row (a + b equal width), bottom row (c)
# =============================================================================
top_row <- p_a + p_b + plot_layout(widths = c(1, 1))
fig2 <- (top_row / p_c) +
  plot_layout(heights = c(1.1, 0.9)) +
  plot_annotation(tag_levels = "a") &
  theme(plot.tag = element_text(size = 12, face = "bold", family = "Helvetica"))

ggsave(file.path(out_dir, "fig2_carryover_pure.pdf"),
       plot = fig2, width = 7.09, height = 6.0, units = "in", device = cairo_pdf)
ggsave(file.path(out_dir, "fig2_carryover_pure.png"),
       plot = fig2, width = 7.09, height = 6.0, units = "in", dpi = 300)

cat("Figure 2 (3-panel, Nature style) saved.\n")
