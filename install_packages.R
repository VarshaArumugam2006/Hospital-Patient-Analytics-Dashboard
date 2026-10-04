# Installation Script for Hospital Patient Analytics Dashboard
# R Packages Installer

cat("=================================================================\n")
cat("  Hospital Patient Analytics Dashboard - Package Installer\n")
cat("=================================================================\n\n")

packages <- c(
  "shiny",
  "bslib",
  "dplyr",
  "ggplot2",
  "plotly",
  "DT",
  "readr",
  "lubridate",
  "tidyr",
  "openxlsx",
  "caret",
  "randomForest"
)

installed <- rownames(installed.packages())
to_install <- packages[!packages %in% installed]

if (length(to_install) > 0) {
  cat("Installing missing packages:\n")
  cat(paste(" -", to_install, collapse = "\n"), "\n\n")
  install.packages(to_install, repos = "https://cloud.r-project.org", type = "binary")
} else {
  cat("All required packages are already installed!\n")
}

cat("\nPackage verification:\n")
for (pkg in packages) {
  avail <- requireNamespace(pkg, quietly = TRUE)
  status <- if (avail) "[OK]" else "[FAILED]"
  cat(sprintf("  %-15s : %s\n", pkg, status))
}

cat("\nReady to launch dashboard with: shiny::runApp()\n")
