# Fear Learning Paradox

Code repository accompanying the manuscript:

**Behavioral summaries obscure reliable learning mechanisms**
Kenny Yu, Wolf Vanpaemel, Maria M. Robinson, Francis Tuerlinckx, Jonas Zaman

## Repository Structure

```
├── shiny_app/          Interactive Shiny application
├── analysis/
│   ├── fig1_reliability_paradox/     Main reliability analysis + model variance panel
│   ├── fig2_carryover_gradient/      Carry-over and generalization simulations
│   ├── fig3_model_misspecification/  Pearce-Hall vs Rescorla-Wagner recovery
│   └── suppfig1_identifiability/     Trial count × reinforcement rate factorial
```

## Interactive Application

An interactive web application allows readers to explore the mechanisms underlying the reliability paradox in fear conditioning. Users can manipulate learning rates, reinforcement schedules, noise levels, and model parameters to observe how behavioral summaries and computational model estimates diverge.

**Live app:** https://kennyccyu.shinyapps.io/fear-learning-paradox/

To run locally:

```r
install.packages(c("shiny", "bslib", "tidyverse", "patchwork"))
shiny::runApp("shiny_app")
```

## Analysis Scripts

All simulations use synthetic participants with parameters drawn from controlled distributions. No external data files are required. Each script is self-contained and can be run independently.

### Figure 1: Reliability paradox + model variance

| Script | Description | Runtime |
|--------|-------------|---------|
| `pure_simulation_analysis.R` | Monte Carlo bootstrap (200 iterations, 6 conditions) | 10-30 min |
| `regen_fig1.R` | Regenerates Fig 1 from saved results (includes model variance panel) | ~30 sec |
| `model_variance_analysis.R` | Standalone model variance demonstration | ~10 sec |

### Figure 2: Carry-over and generalization

| Script | Description | Runtime |
|--------|-------------|---------|
| `carryover_pure.R` | Carry-over gradient simulation (no generalization) | ~15 sec |
| `carryover_with_gen.R` | Carry-over gradient with cross-stimulus generalization | ~15 sec |
| `regen_fig2.R` | Combines results and generates Fig 2 | ~10 sec |

### Figure 3: Model misspecification

| Script | Description | Runtime |
|--------|-------------|---------|
| `model_misspecification_simulation.R` | PH data generation + RW/PH recovery | 2-5 min |

### Supplementary Figure 1: Parameter identifiability

| Script | Description | Runtime |
|--------|-------------|---------|
| `alpha_identifiability_simulation.R` | 7×7 factorial (trials × reinforcement rate) | 3-5 min |

## Dependencies

```r
install.packages(c("tidyverse", "patchwork", "future.apply", "progressr"))
```

All simulations were conducted in R (v4.3+). Parallel computation uses the `future.apply` package.

## Models

Two associative learning models are implemented:

- **Rescorla-Wagner (RW):** Fixed learning rate, prediction error-driven associative update
- **Pearce-Hall (PH):** Dynamic attention-modulated learning rate based on absolute prediction error

Both use a sigmoidal response function mapping associative strengths to observable responses.

## Contact

Kenny Yu — kenny.yu@kuleuven.be
KU Leuven / University of Hasselt, Belgium

## License

This code accompanies a scientific publication. Please cite the associated manuscript if you use or adapt this code.
