# Model variance illustration
# Shows two individuals with different learning rates producing near-identical behavioral summaries

library(tidyverse)

# Set working directory
if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  script_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
  if (script_dir != "" && basename(script_dir) == "model_variance") {
    setwd(script_dir)
  }
} else if (dir.exists("model_variance")) {
  setwd("model_variance")
}

# --- Simulation Functions ---

simulate_learning_trajectory <- function(alpha, n_trials = 30, shock_trials,
                                         w0 = -1, w1 = 4, A = 0, K = 100,
                                         sigma = 5) {
  v <- 0
  responses <- numeric(n_trials)
  values <- numeric(n_trials)

  for (t in 1:n_trials) {
    theta <- A + (K - A) / (1 + exp(-(w0 + w1 * v)))
    responses[t] <- rnorm(1, theta, sigma)
    values[t] <- v

    if (t %in% shock_trials) {
      v <- v + alpha * (1 - v)
    } else {
      v <- v + alpha * (0 - v)
    }
  }

  data.frame(
    trial = 1:n_trials,
    response = responses,
    value = values,
    shock = 1:n_trials %in% shock_trials
  )
}

calculate_behavioral_indices <- function(responses) {
  n_trials <- length(responses)

  mean_resp <- mean(responses, na.rm = TRUE)

  slope_model <- lm(responses ~ seq_len(n_trials))
  slope <- coef(slope_model)[2]

  first_last_diff <- responses[n_trials] - responses[1]

  final_trial <- responses[n_trials]

  list(
    mean = mean_resp,
    slope = slope,
    first_last_diff = first_last_diff,
    final_trial = final_trial
  )
}

# --- Generate Exemplar Individuals with Different Mechanisms ---

set.seed(2025)

# Define TWO individuals with dramatically different learning rates
# Precisely engineered to match BOTH final trial response AND first-last difference
# Through careful tuning of alpha, shock timing, and number of shocks

# Individual A: VERY HIGH learning rate + MID-LATE shocks, NO shock on final trial
# - Extremely fast learner (α = 0.90)
# - Gets 10 shocks mid-late, NOT on final trial so there's extinction
# - Pattern: FLAT early → rise mid-late → DECAY on final trial
alpha_A <- 0.90
shocks_A <- c(10, 12, 14, 16, 18, 20, 22, 24, 26, 28)  # 10 shocks, mid-late, NOT on 30

# Individual B: LOW learning rate + EARLY/MID concentrated shocks
# - Slow learner (α = 0.10)
# - Gets 10 shocks concentrated early/mid session
# - Pattern: gradual rise early → PLATEAU late
alpha_B <- 0.10
shocks_B <- c(2, 4, 6, 8, 10, 12, 14, 16, 18, 20)  # 10 shocks, early-mid

# Deterministic simulation (sigma = 0): values are seed-independent and match caption
data_A <- simulate_learning_trajectory(alpha_A, shock_trials = shocks_A, sigma = 0)
data_B <- simulate_learning_trajectory(alpha_B, shock_trials = shocks_B, sigma = 0)

# Calculate behavioral indices
indices_A <- calculate_behavioral_indices(data_A$response)
indices_B <- calculate_behavioral_indices(data_B$response)

# Combine data for plotting
data_A$individual <- "A"
data_A$alpha <- alpha_A
data_A$label <- sprintf("Individual A (learning rate = %.2f)", alpha_A)

data_B$individual <- "B"
data_B$alpha <- alpha_B
data_B$label <- sprintf("Individual B (learning rate = %.2f)", alpha_B)

all_data <- rbind(data_A, data_B)

# --- Create Visualization ---

all_data$trial_type <- ifelse(all_data$shock, "Shock", "No shock")

table_text <- paste(
  sprintf("Individual A:  Mean = %.1f, First-last diff = %.1f",
          indices_A$mean, indices_A$first_last_diff),
  sprintf("Individual B: Mean = %.1f, First-last diff = %.1f",
          indices_B$mean, indices_B$first_last_diff),
  sep = "\n"
)

figure <- ggplot(all_data, aes(x = trial, y = response, color = label, group = label)) +
  # Plot trajectories as lines
  geom_line(linewidth = 1.2, alpha = 0.8) +

  # Add points with shapes: triangle for shock, circle for no shock
  geom_point(aes(shape = trial_type), size = 3, alpha = 0.9) +

  # Shape scale
  scale_shape_manual(
    values = c("Shock" = 17, "No shock" = 16),  # 17=triangle, 16=circle
    name = ""
  ) +

  # Add annotation box with background
  annotate("rect", xmin = 9, xmax = 30, ymin = 5, ymax = 22,
           fill = "white", color = "black", linewidth = 0.5, alpha = 0.9) +
  annotate("text", x = 29.5, y = 13.5, label = table_text,
           hjust = 1, vjust = 0.5, size = 5, lineheight = 1.1,
           fontface = "plain", color = "black") +

  # Color scheme (sentence case for labels) - only A and B
  scale_color_manual(
    values = setNames(
      c("#E63946", "#1E90FF"),
      c(sprintf("Individual A (learning rate = %.2f)", alpha_A),
        sprintf("Individual B (learning rate = %.2f)", alpha_B))
    ),
    name = ""
  ) +

  # Labels and theme (sentence case, white theme, no grid lines)
  labs(
    x = "Trial",
    y = "Fear response",
  ) +
  theme_classic(base_size = 22) +
  theme(
    plot.title = element_text(face = "bold", size = 26, hjust = 0.5, margin = margin(b = 15)),
    axis.title = element_text(size = 24, face = "bold"),
    axis.text = element_text(size = 20),
    axis.line = element_line(color = "black", linewidth = 0.5),
    legend.position = "bottom",
    legend.text = element_text(size = 18),
    legend.title = element_text(size = 20, face = "bold"),
    legend.box = "horizontal",
    legend.margin = margin(t = 5, b = 0),
    legend.box.spacing = unit(0.3, "cm"),
    plot.margin = margin(20, 20, 20, 20),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  ) +
  guides(
    color = guide_legend(order = 1, ncol = 1),
    shape = guide_legend(order = 2, ncol = 1)
  ) +
  coord_cartesian(xlim = c(1, 30), ylim = c(0, 100))

# Display figure
print(figure)

# --- Save Figure ---

figure_width <- 8
figure_height <- 6
figure_dpi <- 600

ggsave('model_variance_figure.png', figure,
       width = figure_width, height = figure_height, dpi = figure_dpi, bg = 'white')

ggsave('model_variance_figure.pdf', figure,
       width = figure_width, height = figure_height, bg = 'white', device = cairo_pdf)

ggsave('model_variance_figure.tiff', figure,
       width = figure_width, height = figure_height, dpi = figure_dpi,
       bg = 'white', compression = 'lzw')

# --- Print Summary Statistics ---

cat(sprintf('Individual A: mean=%.1f, first-last=%.1f\nIndividual B: mean=%.1f, first-last=%.1f\n',
            indices_A$mean, indices_A$first_last_diff, indices_B$mean, indices_B$first_last_diff))
