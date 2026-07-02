# Assemble Fig 3 (reliability paradox) and Fig 4 (variance decomposition) from cached panels.
suppressPackageStartupMessages({
  library(ggplot2); library(dplyr); library(tidyr); library(patchwork); library(scales)
})

res_dir <- file.path(dirname(getwd()), "results")
fig_dir <- file.path(dirname(getwd()), "figures")

# reliability-paradox panels (reliability_paradox.R)
panel_traj <- readRDS(file.path(res_dir, "panel_paradox_traj.rds"))
fig_icc    <- readRDS(file.path(res_dir, "panel_paradox_icc.rds"))

# variance-decomposition panels (decomposition.R)
vres <- readRDS(file.path(res_dir, "variance_decomposition_improved_results.rds"))
vd <- vres$vd; icc_df <- vres$icc; sb <- vres$sb

vd_long <- vd %>%
  select(n_trials, prop_trait, prop_sequence, prop_noise) %>%
  pivot_longer(-n_trials, names_to = "component", values_to = "proportion") %>%
  mutate(component = factor(component,
    levels = c("prop_trait", "prop_sequence", "prop_noise"),
    labels = c("Learning rate", "Trial order", "Measurement noise")))

p_decomp <- ggplot(vd_long, aes(x = factor(n_trials), y = proportion, fill = component)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.7) +
  scale_fill_manual(values = c(
    "Learning rate"     = "#2ca02c",
    "Trial order"       = "#d62728",
    "Measurement noise" = "#7f7f7f")) +
  scale_y_continuous(labels = scales::percent, expand = c(0, 0.01), limits = c(0, 0.8)) +
  labs(x = "Trials per session", y = "Share of the slope's variation",
       fill = NULL, tag = "a") +
  theme_bw(base_size = 10) +
  theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
        legend.position = "bottom", legend.key.height = unit(0.3, "cm"),
        legend.key.width = unit(0.5, "cm"), legend.text = element_text(size = 8),
        plot.tag = element_text(face = "bold", size = 12))

icc_long <- bind_rows(
  icc_df %>% transmute(n_trials, icc = icc_alpha_mean, lo = icc_alpha_lo, hi = icc_alpha_hi, method = "Model-based"),
  icc_df %>% transmute(n_trials, icc = icc_slope_mean, lo = icc_slope_lo, hi = icc_slope_hi, method = "Slope"),
  icc_df %>% transmute(n_trials, icc = icc_fl_mean,    lo = icc_fl_lo,    hi = icc_fl_hi,    method = "First-to-last change"),
  icc_df %>% transmute(n_trials, icc = icc_last_mean,  lo = icc_last_lo,  hi = icc_last_hi,  method = "Final response"),
  sb %>% transmute(n_trials, icc = icc_sb, lo = NA_real_, hi = NA_real_, method = "Spearman-Brown"))
icc_long$method <- factor(icc_long$method,
  levels = c("Model-based", "Slope", "First-to-last change", "Final response", "Spearman-Brown"))
mcol <- c("Model-based"="#1B7837","Slope"="#D62728","First-to-last change"="#E6550D",
          "Final response"="#756BB1","Spearman-Brown"="#969696")
mline <- c("Model-based"="solid","Slope"="solid","First-to-last change"="solid",
           "Final response"="solid","Spearman-Brown"="dashed")

p_trialcount <- ggplot(icc_long, aes(x = n_trials, y = icc, color = method, linetype = method)) +
  geom_ribbon(aes(ymin = lo, ymax = hi, fill = method), alpha = 0.10, color = NA, na.rm = TRUE) +
  geom_line(linewidth = 0.8) +
  geom_point(data = filter(icc_long, method != "Spearman-Brown"), size = 1.5) +
  scale_color_manual(values = mcol, name = NULL) +
  scale_fill_manual(values = mcol, guide = "none") +
  scale_linetype_manual(values = mline, name = NULL) +
  scale_x_log10(breaks = c(20, 50, 100, 200, 500, 1000)) +
  scale_y_continuous(limits = c(-0.1, 1.02), breaks = seq(0, 1, 0.2)) +
  labs(x = "Trials per session (log scale)", y = "Test-retest reliability", tag = "d") +
  theme_bw(base_size = 10) +
  theme(panel.grid.minor = element_blank(), legend.position = "bottom",
        legend.box.margin = margin(t = -4), legend.key.height = unit(0.3, "cm"),
        legend.key.width = unit(0.6, "cm"), legend.text = element_text(size = 8),
        plot.tag = element_text(face = "bold", size = 12))

# eight-summary decomposition panel (decomposition.R)
nres <- readRDS(file.path(res_dir, "nuisance_decomposition_results.rds"))
decomp <- nres$decomposition; icc_results <- nres$icc

nice_names <- c(slope="Slope", delta_fl="First-to-last change", last_trial="Final response",
  mean_csplus="Mean CS+", mean_csminus="Mean CS−", diff_score="CS+ minus CS−",
  last_block_diff="End-of-session difference", mean_all="Mean response")
level_labels <- c("Measurement noise", "Trial order", "Response sensitivity",
                  "Baseline responsivity", "Learning rate")

plot_df <- decomp %>%
  pivot_longer(-summary, names_to = "source", values_to = "proportion") %>%
  mutate(summary = factor(nice_names[summary], levels = rev(nice_names)),
         source  = factor(source, levels = c("noise","context","w1","w0","alpha"),
                          labels = level_labels))
source_colors <- setNames(c("#2ca02c","#9467bd","#ff7f0e","#d62728","#7f7f7f"),
                          rev(level_labels))
icc_nui <- data.frame(summary = factor(nice_names[names(icc_results)], levels = rev(nice_names)),
                      icc = as.numeric(icc_results))

p_nui <- ggplot(plot_df, aes(x = proportion, y = summary, fill = source)) +
  geom_col(width = 0.7) +
  geom_point(data = icc_nui, aes(x = icc, y = summary), inherit.aes = FALSE,
             shape = 23, size = 3.2, fill = "white", color = "black", stroke = 1) +
  scale_fill_manual(values = source_colors, name = "Source of variation") +
  scale_x_continuous(limits = c(0, 1.05), breaks = seq(0, 1, 0.25), expand = c(0, 0)) +
  labs(x = "Share of the summary's variation (bars)   /   test-retest reliability (diamonds)",
       y = NULL, tag = "b") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom", panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(), plot.tag = element_text(face = "bold", size = 12),
        legend.text = element_text(size = 8.5), legend.title = element_text(size = 9),
        legend.key.size = unit(0.32, "cm"), plot.margin = margin(5, 12, 5, 5)) +
  guides(fill = guide_legend(nrow = 2, reverse = TRUE))

# Fig 3: trajectories (a) / heatmaps (b,c) / trial-count (d)
part1 <- panel_traj / fig_icc / p_trialcount + plot_layout(heights = c(1, 2.3, 1.15))
ggsave(file.path(fig_dir, "fig3_reliability_paradox.pdf"), part1,
       width = 7, height = 9, bg = "white", device = cairo_pdf, units = "in")

# Fig 4: slope decomposition (a) / eight-summary decomposition (b)
part2 <- p_decomp / p_nui + plot_layout(heights = c(1, 1.3))
ggsave(file.path(fig_dir, "fig4_variance_decomposition.pdf"), part2,
       width = 8.4, height = 8, bg = "white", device = cairo_pdf, units = "in")
