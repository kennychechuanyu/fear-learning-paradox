# Launch Behavior-Mechanism Gap Demo

required <- c("shiny", "bslib", "tidyverse")
missing <- required[!sapply(required, requireNamespace, quietly = TRUE)]

if (length(missing) > 0) {
  message("Installing: ", paste(missing, collapse = ", "))
  install.packages(missing)
}

if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}

shiny::runApp(launch.browser = TRUE)
