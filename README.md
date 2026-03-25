# Fear Learning Paradox

Code repository accompanying the manuscript:

**Behavioral summaries obscure reliable learning mechanisms**
Kenny Yu, Maria M. Robinson, Wolf Vanpaemel, Francis Tuerlinckx, Jonas Zaman

## Repository Structure

```
├── shiny_app/          Interactive Shiny application
├── analysis/
│   ├── fig1_reliability_paradox/     Main reliability bootstrap
│   ├── fig2_carryover_gradient/      Carry-over and generalization simulations
│   ├── fig3_model_misspecification/  Pearce-Hall vs Rescorla-Wagner recovery
│   ├── suppfig1_model_variance/     Model variance demonstration
│   └── suppfig2_identifiability/    Trial count × reinforcement rate factorial
```

## Interactive Application

The accompanying Shiny app lets you explore the reliability paradox interactively by manipulating learning rates, reinforcement schedules, noise levels, and model parameters.

**Live app:** https://kennyccyu.shinyapps.io/fear-learning-paradox/

To run locally:

```r
install.packages(c("shiny", "bslib", "tidyverse", "patchwork"))
shiny::runApp("shiny_app")
```

## Analysis Scripts

All simulations are self-contained with no external data dependencies.

### Figure 1: Reliability paradox

| Script | Description | Runtime |
|--------|-------------|---------|
| `pure_simulation_analysis.R` | Monte Carlo bootstrap (200 iterations, 6 conditions) | 10-30 min |
| `behavioral_icc_1000trials.R` | Behavioral ICC at 1000 trials per session | ~2 min |

### Figure 2: Carry-over and generalization

| Script | Description | Runtime |
|--------|-------------|---------|
| `carryover_pure.R` | Carry-over gradient simulation (no generalization) | ~15 sec |
| `carryover_with_gen.R` | Carry-over gradient with cross-stimulus generalization | ~15 sec |

### Figure 3: Model misspecification

| Script | Description | Runtime |
|--------|-------------|---------|
| `model_misspecification_simulation.R` | PH data generation + RW/PH recovery | 2-5 min |

### Supplementary Figure 1: Model variance

| Script | Description | Runtime |
|--------|-------------|---------|
| `model_variance_analysis.R` | Two exemplar participants with different learning rates | ~10 sec |

### Supplementary Figure 2: Parameter identifiability

| Script | Description | Runtime |
|--------|-------------|---------|
| `alpha_identifiability_simulation.R` | 7×7 factorial (trials × reinforcement rate) | 3-5 min |

## Dependencies

```r
install.packages(c("tidyverse", "patchwork", "future.apply", "progressr"))
```

R (v4.3+). Parallel computation uses `future.apply`.

## Models

Two associative learning models are implemented:

- **Rescorla-Wagner (RW):** Fixed learning rate, prediction error-driven update
- **Pearce-Hall (PH):** Dynamic attention-modulated learning rate

Both use a sigmoidal response function mapping associative strengths to observable responses.

## Contact

Kenny Yu — kenny.yu@kuleuven.be
KU Leuven / University of Hasselt, Belgium

## License

This code accompanies a scientific publication. Please cite the associated manuscript if you use or adapt this code.
