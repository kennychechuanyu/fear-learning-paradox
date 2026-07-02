# Analysis code

Run scripts from inside `analysis/`; they use relative paths (`../results`, `../figures`, `../data`). Simulations set a seed and write cached results to `../results/`; a re-run skips any simulation whose cache already exists and rebuilds the figure from it.

## Figures

| Figure | File | Script | Cached inputs |
|---|---|---|---|
| 1 | `fig1_framework.pdf` | TikZ (`fig1_framework_src.tex`, compiled with pdflatex) | n/a (schematic, no data) |
| 2 | `fig2_nonInvertibility.pdf` | `non_invertibility.R` | self-contained (seeded) |
| 3 | `fig3_reliability_paradox.pdf` | `assemble_paradox_figures.R` | `panel_paradox_traj.rds`, `panel_paradox_icc.rds` (`reliability_paradox.R`); `variance_decomposition_improved_results.rds` (`decomposition.R`) |
| 4 | `fig4_variance_decomposition.pdf` | `assemble_paradox_figures.R` | `variance_decomposition_improved_results.rds`, `nuisance_decomposition_results.rds` (`decomposition.R`) |
| 5 | `fig5_misspecification_crossonly.pdf` | `misspecification.R` | `cross_model_recovery_results.RData` |
| 6 | `fig6_carryover_gradient.pdf` | `carryover_generalization.R` | `carryover_results.rds` |
| 7 | `fig7_design.pdf` | `design_recovery.R` | `alpha_identifiability_results.RData` |
| 8 | `fig8_empirical_anchor.pdf` | `empirical.R` | `igt_compression_results.rds` + acquisition data (`empirical.R`) |
| S1 | `suppfig1_drift_robustness.pdf` | `supplementary_analyses.R` | `drift_robustness_results.RData` |
| S2 | `suppfig2_hbayes_vs_mle.pdf` | `supplementary_analyses.R` | `hbayes_vs_mle_results.rds` |

## The nine scripts

(Figure 1, the framework schematic, is the TikZ file `figures/fig1_framework_src.tex`, not produced by these scripts.)

- `non_invertibility.R` — Fig 2, the four-participant non-invertibility witness (self-contained, seeded).
- `reliability_paradox.R` — the rate-only reliability bootstrap; writes the Fig 3a–c panel caches.
- `decomposition.R` — slope and eight-summary variance decompositions; writes the Fig 3d / Fig 4 caches. (Slowest script: the ICC bootstraps take roughly an hour.)
- `assemble_paradox_figures.R` — assembles Fig 3 and Fig 4 from the caches above.
- `misspecification.R` — cross-model (Rescorla–Wagner vs Pearce–Hall) recovery; writes Fig 5.
- `carryover_generalization.R` — carry-over and generalization reliability gradient (Fig 6).
- `design_recovery.R` — design factorial recovery map (Fig 7).
- `empirical.R` — the Fig 8 pipeline in one file: compression-index helpers, acquisition/IGT data prep, and the two-scatter figure (coupling + a permutation-floor significance test).
- `supplementary_analyses.R` — drift robustness (Supp 1) and empirical-Bayes vs MLE (Supp 2).

## Run order

1. Run the cache-producing scripts: `reliability_paradox.R`, `decomposition.R` (and the cache-guarded sims inside `misspecification.R`, `carryover_generalization.R`, `design_recovery.R`, `supplementary_analyses.R`, `empirical.R`).
2. Run the figure scripts: `non_invertibility.R`, `assemble_paradox_figures.R`, `misspecification.R`, `carryover_generalization.R`, `design_recovery.R`, `empirical.R`, `supplementary_analyses.R`.
3. `cd ..` and build: `pdflatex main.tex && bibtex main && pdflatex main.tex && pdflatex main.tex`.

`archive/` (under `analysis/`, `results/`, and `figures/`) holds superseded scripts, orphan caches, and additional exploratory analyses that are not part of the manuscript: a decoder-comparison analysis, a generalization variance-decomposition (`empirical_generalization.R`), an RW/PH mixture gradient, and the acquisition-identifiability fits.
