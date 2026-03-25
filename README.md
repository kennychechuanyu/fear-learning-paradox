# Fear Learning Paradox: Interactive Shiny Application

An interactive web application accompanying the manuscript:

**Behavioral summaries obscure reliable learning mechanisms**
Kenny Yu, Wolf Vanpaemel, Maria M. Robinson, Francis Tuerlinckx, Jonas Zaman

## Overview

This application allows users to explore how behavioral summary statistics and computational model estimates diverge in fear conditioning paradigms. Users can manipulate learning rates, reinforcement schedules, noise levels, and model parameters to observe how:

- Different learning rates and reinforcement schedules can produce identical behavioral summaries (the many-to-one mapping problem)
- Computational models recover latent parameters from trial-by-trial data
- Auxiliary processes (memory carry-over, cross-stimulus generalization) degrade both behavioral and model-based measurement

## Live Application

The app is deployed at: https://kennyccyu.shinyapps.io/fear-learning-paradox/

## Running Locally

```r
# Install required packages
install.packages(c("shiny", "bslib", "tidyverse", "patchwork"))

# Run the app
shiny::runApp()
```

## Structure

- `app.R` -- Main Shiny application (UI and server logic)
- `functions/rw_core.R` -- Core simulation and recovery functions (Rescorla-Wagner, Pearce-Hall models, MLE recovery, ICC computation)
- `launch_app.R` -- Helper script for launching the app

## Models

The application implements two associative learning models:

- **Rescorla-Wagner (RW):** Fixed learning rate, prediction error-driven update
- **Pearce-Hall (PH):** Dynamic attention-modulated learning rate

Both use a sigmoidal response function mapping associative strengths to observable responses.

## Contact

Kenny Yu -- kenny.yu@kuleuven.be
KU Leuven / University of Hasselt, Belgium

## License

This code accompanies a scientific publication. Please cite the associated manuscript if you use or adapt this code.
