# Run from the repository root. Training dependencies are opt-in.
training <- "--training" %in% commandArgs(trailingOnly = TRUE)
packages <- c("shiny", "bslib", "DT", "ggplot2", "readr", "testthat", "jsonlite", "renv")
if (training) packages <- c(packages, "tidymodels", "xgboost")
missing <- setdiff(packages, rownames(installed.packages()))
if (length(missing)) install.packages(missing, repos = "https://cloud.r-project.org")
message("Dependencies installed. Run shiny::runApp('.') from the repository root.")
